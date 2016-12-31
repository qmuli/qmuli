{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.LambdaDispatcher (invokeLambda) where

import           Control.Lens
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Reader          (ReaderT, ask)
import           Data.Aeson                          (eitherDecode)
import           Data.Aeson.Types                    (parseEither)
import qualified Data.ByteString.Lazy.Char8          as LBS
import qualified Data.HashMap.Strict                 as SHM
import           Data.Text                           (Text)
import qualified Data.Text                           as T

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


invokeLambda
  :: String
  -> String
  -> ReaderT Config IO ()
invokeLambda name event = do
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



