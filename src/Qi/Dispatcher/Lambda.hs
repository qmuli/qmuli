{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Dispatcher.Lambda (invoke, update) where

import           Control.Lens
import           Control.Monad                       (forM_)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader          (ReaderT, ask)
import           Data.Aeson                          (Value, eitherDecode)
import           Data.Aeson.Types                    (parseEither)
import qualified Data.ByteString.Lazy.Char8          as LBS
import qualified Data.HashMap.Strict                 as SHM
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Network.AWS                         (AWS, send)
import           Network.AWS.Lambda                  (uS3Bucket, uS3Key,
                                                      updateFunctionCode)

import           Qi.Config.AWS
import qualified Qi.Config.AWS.ApiGw.ApiMethod.Event as ApiMethodEvent (parse)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event              as S3Event
import           Qi.Program.Lambda.Interface         (CompleteLambdaProgram)
import qualified Qi.Program.Lambda.Interpreters.IO   as LIO


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
  :: String
  -> String
  -> ReaderT Config IO ()
invoke name event = do
  config <- ask
  liftIO $ do
    case SHM.lookup (T.pack name) $ lbdIOMap config of
      Nothing ->
        putStrLn $ "No lambda with name '" ++ name ++ "' was found"
      Just lbdIO ->
        lbdIO event

lbdIOMap config = SHM.fromList $ map toLbdIOPair $ getAll config
  where
    toLbdIOPair
      :: Lambda
      -> (Text, String -> IO ())
    toLbdIOPair lbd = (name, lbdIO name lbd)
      where
        name = lbd^.lbdName

        lbdIO
          :: Text
          -> Lambda
          -> String
          -> IO ()
        lbdIO name lbd eventJson =
          either
            (\err -> fail $ concat ["Could not parse event: ", show eventJson, ", error was: ", err])
            (LIO.run name config)
            (parseLambdaEvent lbd eventJson)

    parseLambdaEvent
      :: Lambda
      -> String
      -> Either String CompleteLambdaProgram

    parseLambdaEvent GenericLambda{_lbdGenericLambdaProgram} eventJson =
      _lbdGenericLambdaProgram <$> (eitherDecode (LBS.pack eventJson) :: Either String Value)

    parseLambdaEvent S3BucketLambda{_lbdS3BucketLambdaProgram} eventJson =
      _lbdS3BucketLambdaProgram <$> (parseEither (`S3Event.parse` config) =<< eitherDecode (LBS.pack eventJson))

    parseLambdaEvent ApiLambda{_lbdApiMethodLambdaProgram} eventJson =
      _lbdApiMethodLambdaProgram <$> (parseEither (`ApiMethodEvent.parse` config) =<< eitherDecode (LBS.pack eventJson))

    parseLambdaEvent CfCustomLambda{_lbdCfCustomLambdaProgram} eventJson =
      _lbdCfCustomLambdaProgram <$> (eitherDecode (LBS.pack eventJson) :: Either String CfEvent)

    parseLambdaEvent CwEventLambda{_lbdCwEventLambdaProgram} eventJson =
      _lbdCwEventLambdaProgram <$> (eitherDecode (LBS.pack eventJson) :: Either String CwEvent)

    parseLambdaEvent DdbStreamLambda{_lbdDdbStreamLambdaProgram} eventJson =
      _lbdDdbStreamLambdaProgram <$> (eitherDecode (LBS.pack eventJson) :: Either String DdbStreamEvent)



