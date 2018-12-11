{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Qi.AWS.Runtime where

import           Protolude             hiding (get, try)

import           Control.Monad.Catch   (MonadCatch, MonadThrow, throwM, try)
import           Data.Aeson
import           Data.Default.Class    (def)
import           Data.Maybe            (fromJust)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as TextEncoding
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Network.HTTP.Req      ((/:))
import qualified Network.HTTP.Req      as Req
import           System.Environment    (lookupEnv)


data HandlerRequest = HandlerRequest
  { payload         :: Text
  , requestId       :: Text
  , xrayTraceId     :: Maybe Text
  , clientContext   :: Maybe Text
  , cognitoIdentity :: Maybe Text
  , functionArn     :: Maybe Text
  , deadline        :: Maybe UTCTime
  }

----------------------------------------------------------------
data HandlerResponse
  = SuccessHandlerResponse { mPayload     :: Text
                           , mContentType :: Maybe Text }
  | FailureHandlerResponse { mErrorMsg  :: Text
                           , mErrorType :: Text }

isSuccess :: HandlerResponse -> Bool
isSuccess SuccessHandlerResponse {} = True
isSuccess FailureHandlerResponse {} = False

getPayload :: HandlerResponse -> Text
getPayload rsp@SuccessHandlerResponse {} = mPayload rsp
getPayload rsp@FailureHandlerResponse {} = toS $ encode v
  where
    v :: Value
    v =
      object
        [ ("errorMessage", String (mErrorMsg rsp))
        , ("errorType", String (mErrorType rsp))
        , ("stackTrace", Array empty)
        ]

mkSuccess :: Text -> Text -> HandlerResponse
mkSuccess p ct = SuccessHandlerResponse {mPayload = p, mContentType = Just ct}

mkFailure :: Text -> Text -> HandlerResponse
mkFailure errorMsg errorType =
  FailureHandlerResponse {mErrorMsg = errorMsg, mErrorType = errorType}

----------------------------------------------------------------


newtype ErrorCode =
  ErrorCode Int
  deriving (Show)

data Response a
  = SuccessResponse a
  | ErrorResponse ErrorCode

mkErrorResponse :: Int -> Response a
mkErrorResponse code = ErrorResponse (ErrorCode code)

mkSuccessResponse :: a -> Response a
mkSuccessResponse = SuccessResponse

isSuccessCode :: Int -> Bool
isSuccessCode code = code >= 200 && code <= 299


----------------------------------------------------------------

getEndpoint
  :: (MonadIO m, MonadThrow m, MonadCatch m)
  => m (Either Text (Text, Int))
getEndpoint = liftIO $ do
  maybe
    (Left "AWS_LAMBDA_RUNTIME_API not found in ENV")
    (Right . getHostAndPort . toS)
  <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"
  where
    getHostAndPort endpoint =
      let getPort rawPort = fromMaybe 80 . readMaybe . toS $ rawPort
       in case Text.splitOn ":" endpoint of
            []              -> ("", 80)
            [host]          -> (host, 80)
            [host, portRaw] -> (host, getPort portRaw)
            host:portRaw:_  -> (host, getPort portRaw)

---------------------------------------------------------------


getWithRetries
  :: forall m. (MonadThrow m, MonadCatch m, MonadIO m)
  => Int
  -> (Text, Int)
  -> m (Response HandlerRequest)
getWithRetries maxRetries endpoint = getWithRetries' 0
  where
    getWithRetries' retryNum = do
      rsp <-
        try (get endpoint) :: m (Either SomeException (Response HandlerRequest))
      either handleError pure rsp
      where
        handleError ex
          | retryNum >= maxRetries = throwM ex
          | otherwise = getWithRetries' $ retryNum + 1


get
  :: MonadIO m
  => (Text, Int)
  -> m (Response HandlerRequest)
get (host, port) = do
  let url = Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: "next"
  rsp' <-
    Req.runReq def $ do
      rsp <-
        Req.req
          Req.GET
          url
          Req.NoReqBody
          Req.bsResponse
          (Req.port port <> Req.responseTimeout 3000000)
      let code = Req.responseStatusCode rsp
      pure $
        if not (isSuccessCode code)
          then mkErrorResponse code
          else let buildResponse reqId =
                     mkSuccessResponse
                       HandlerRequest
                         { payload =
                             TextEncoding.decodeUtf8 . Req.responseBody $ rsp
                         , requestId = reqId
                         , xrayTraceId = getHeader rsp "lambda-runtime-trace-id"
                         , clientContext =
                             getHeader rsp "lambda-runtime-client-context"
                         , cognitoIdentity =
                             getHeader rsp "lambda-runtime-cognito-identity"
                         , functionArn =
                             getHeader rsp "lambda-runtime-invoked-function-arn"
                         , deadline = getDeadline rsp
                         }
                in maybe
                     (mkErrorResponse (-1))
                     buildResponse
                     (TextEncoding.decodeUtf8 <$>
                      Req.responseHeader rsp "lambda-runtime-aws-request-id")
  {- logResponse rsp' -}
  pure rsp'
  where
    {- logResponse (SuccessResponse req) = -}
      {- $(logDebug) -}
        {- ("Success to get next invocation. ReqId is" <> (show . requestId $ req)) -}
    {- logResponse (ErrorResponse code) = -}
      {- $(logDebug) -}
        {- ("Failed to get next invocation. Http Response code: " <> show code) -}
    getHeader r header = TextEncoding.decodeUtf8 <$> Req.responseHeader r header
    getDeadline r = do
      millis <-
        (Text.unpack <$> getHeader r "lambda-runtime-deadline-ms" >>= readMaybe) :: Maybe Int
      Just $ posixSecondsToUTCTime (fromIntegral millis / 1000)

-----------------------------------------------------------------

getUrl :: Text -> Text -> HandlerResponse -> Req.Url 'Req.Http
getUrl host reqId SuccessHandlerResponse {} =
  Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /:
  "response"
getUrl host reqId FailureHandlerResponse {} =
  Req.http host /: "2018-06-01" /: "runtime" /: "invocation" /: reqId /: "error"

respond
  :: MonadIO m
  => (Text, Int)
  -> Text
  -> HandlerResponse
  -> m ()
respond (host, port) reqId handlerRsp = do
  let url = getUrl host reqId handlerRsp
  rsp <- doPost url port reqId handlerRsp
  void $ handleResponse reqId rsp
  where
    handleResponse reqId (SuccessResponse _) = do
      return True
    -- TODO: Improve handling for this scenario by reporting it to the runtime/init/error endpoint
    -- See https://github.com/awslabs/aws-lambda-rust-runtime/blob/ad28790312219fb63f26170ae0d8be697fc1f7f2/lambda-runtime/src/runtime.rs#L12 fail_init
    -- Also see https://github.com/awslabs/aws-lambda-rust-runtime/blob/master/lambda-runtime-client/src/client.rs
    handleResponse reqId (ErrorResponse code) = do
      {- $(logWarn) -}
        {- ("HTTP Request for invocation" <> reqId <> -}
         {- "was not successful. HTTP response code: " <> -}
         {- show code) -}
      return False

doPost
  :: MonadIO m
  => Req.Url scheme
  -> Int
  -> Text
  -> HandlerResponse
  -> m (Response ())
doPost url port reqId handlerRsp = do
  Req.runReq def $ do
    let payload =
          TextEncoding.encodeUtf8 . getPayload $ handlerRsp
    let contentTypeHeader =
          Req.header "content-type" $
          TextEncoding.encodeUtf8 $
          maybe "text/html" identity (mContentType handlerRsp)
    let options =
          contentTypeHeader <> Req.port port <> Req.responseTimeout 3000000
    rsp <-
      Req.req Req.POST url (Req.ReqBodyBs payload) Req.ignoreResponse options
    let code = Req.responseStatusCode rsp
    pure $
      if not (isSuccessCode code)
        then mkErrorResponse code
        else mkSuccessResponse ()
