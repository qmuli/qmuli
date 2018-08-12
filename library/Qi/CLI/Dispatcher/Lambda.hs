{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.CLI.Dispatcher.Lambda (invoke, update, logs) where

import           Control.Lens
import           Data.Aeson                           (Value, eitherDecode,
                                                       encode)
import           Data.Aeson.Types                     (parseEither)
import qualified Data.ByteString.Lazy.Char8           as LBS
import qualified Data.HashMap.Strict                  as SHM
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Network.AWS                          (AWS, send)
import           Network.AWS.Lambda                   (uS3Bucket, uS3Key,
                                                       updateFunctionCode)
import           Protolude                            hiding (getAll)

import           Qi.Config.AWS
import qualified Qi.Config.AWS.ApiGw.ApiMethod.Event  as ApiMethodEvent (parse)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CfCustomResource.Types (CfCustomResourceEvent)
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event               as S3Event
import qualified Qi.Program.Lambda.Interpreters.IO    as LIO


update
  :: Text
  -> [Text]
  -> AWS ()
update appName names =
  forM_ names $ \name ->
    send $ updateFunctionCode name
            & uS3Bucket ?~ appName
            & uS3Key ?~ "lambda.zip"

invoke
  :: Text
  -> Text
  -> ReaderT Config IO ()
invoke name evt = do
  config <- ask
  liftIO $ do
    case SHM.lookup name $ lbdIOMap config of
      Nothing ->
        putStrLn $ "No lambda with name '" <> name <> "' was found"
      Just lbdIO ->
        putStr =<< lbdIO evt


logs
  :: Text
  -> ReaderT Config IO ()
logs _name =
  -- send $
  pass


lbdIOMap
  :: Config
  -> SHM.HashMap Text (Text -> IO LBS.ByteString)
lbdIOMap config = SHM.fromList $ map toLbdIOPair $ getAll config
  where
    toLbdIOPair
      :: Lambda
      -> (Text, Text -> IO LBS.ByteString)
    toLbdIOPair l = (name, lbdIO name l)
      where
        name = l ^. lbdName

        lbdIO
          :: Text
          -> Lambda
          -> Text
          -> IO LBS.ByteString
        lbdIO name' lbd eventJson =
          either
            (\err -> panic $ "Could not parse event: " <> eventJson <> ", error was: " <> toS err)
            identity
            (parseLambdaEvent lbd)

    {- parseLambdaEvent -}
      {- :: Lambda -}
      {- -> Text -}
      {- -> Either [Char] (Eff effs LBS.ByteString) -}
    {- parseLambdaEvent = panic "parseLambdaEvent is unimplemented" -}
-- TODO: rethink this

          where
            parseLambdaEvent
              :: Lambda
              -> Either [Char] (IO LBS.ByteString)

            parseLambdaEvent GenericLambda{_lbdEffsProxy, _lbdGenericLambdaProgram} =
              fmap encode . LIO.runLambdaProgram name' config LIO.StdOutLogger _lbdEffsProxy . _lbdGenericLambdaProgram
                <$> eitherDecode (toS eventJson)
            parseLambdaEvent S3BucketLambda{_lbdEffsProxy, _lbdS3BucketLambdaProgram} =
              LIO.runLambdaProgram name' config LIO.StdOutLogger _lbdEffsProxy . _lbdS3BucketLambdaProgram
                <$> (parseEither (S3Event.parse config) =<< eitherDecode (toS eventJson))

{-
    parseLambdaEvent ApiLambda{_lbdApiMethodLambdaProgram} eventJson =
      _lbdApiMethodLambdaProgram <$> (parseEither (ApiMethodEvent.parse config) =<< eitherDecode (toS eventJson))

    parseLambdaEvent CfCustomLambda{_lbdCfCustomLambdaProgram} eventJson =
      _lbdCfCustomLambdaProgram <$> (eitherDecode (toS eventJson) :: Either [Char] CfCustomResourceEvent)

    parseLambdaEvent CwEventLambda{_lbdCwEventLambdaProgram} eventJson =
      _lbdCwEventLambdaProgram <$> (eitherDecode (toS eventJson) :: Either [Char] CwEvent)

    parseLambdaEvent DdbStreamLambda{_lbdDdbStreamLambdaProgram} eventJson =
      _lbdDdbStreamLambdaProgram <$> (eitherDecode (toS eventJson) :: Either [Char] DdbStreamEvent)

-}

