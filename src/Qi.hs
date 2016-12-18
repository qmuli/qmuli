{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Char                            (isDigit, isLower)
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           System.Environment                   (getArgs, withArgs)

import           Qi.Config.AWS
import qualified Qi.Config.AWS.Api.Event              as ApiEvent (parse)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.Lambda.Accessors       (getAllLambdas)
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event               as S3Event (parse)
import           Qi.Config.CF                         as CF
import qualified Qi.Deploy.CF                         as CF
import qualified Qi.Deploy.Lambda                     as Lambda
import qualified Qi.Deploy.S3                         as S3
import           Qi.Program.Config.Interface          (ConfigProgram)
import qualified Qi.Program.Config.Interpreters.Build as CB
import           Qi.Program.Lambda.Interface          (LambdaProgram)
import qualified Qi.Program.Lambda.Interpreters.IO    as LIO


withConfig
  :: ConfigProgram ()
  -> IO ()
withConfig configProgram = do
  args <- getArgs
  case args of
    (appName:rest) -> withArgs rest $ withNameAndConfig (T.pack appName) configProgram
    _              -> putStrLn "Please provide a unique application name for your qmulus"

withNameAndConfig
  :: Text
  -> ConfigProgram ()
  -> IO ()
withNameAndConfig appName configProgram = do

  if invalid appName
    then
      putStrLn $ "Invalid qmulus name: '" ++ T.unpack appName ++ "', the name should only contain alphanumeric lower case characters or a hyphen"
    else do
      args <- getArgs
      case args of
        "cf":"template":[] ->
          LBS.putStr $ CF.render config

        "cf":"deploy":[] -> do -- deploy CF template and the lambda package
          S3.createBucket appName
          S3.upload appName "cf.json" $ CF.render config
          Lambda.deploy appName

        "cf":"create":[] -> -- create CF stack
          CF.create appName

        "cf":"describe":[] -> do -- describe CF stack
          desc <- CF.describe appName
          LBS.putStrLn $ encodePretty desc

        "cf":"destroy":[] -> -- destroy CF stack
          CF.destroy appName


        -- execute the lambda on the event
        "lbd":lbdName:event:_ -> do
          case SHM.lookup (T.pack lbdName) lbdIOMap of
            Nothing ->
              putStrLn $ "No lambda with name '" ++ lbdName ++ "' was found"
            Just lbdIO ->
              lbdIO event


        _ -> putStrLn $ "Unexpected arguments: '" ++ show args ++ "'"

  where
    invalid = not . all (\c -> isLower c || isDigit c || c == '-') . T.unpack

    config = snd . (`runState` def{_namePrefix = appName}) . CB.unQiConfig $ CB.interpret configProgram

    lbdIOMap = SHM.fromList $ map toLbdIOPair $ getAllLambdas config

      where
        toLbdIOPair
          :: Lambda
          -> (Text, String -> IO ())
        toLbdIOPair lbd = (lbd ^. lbdName, lbdIO lbd)

        parseLambdaEvent
          :: Lambda
          -> String
          -> Either String (LambdaProgram ())

        parseLambdaEvent S3BucketLambda{_lbdS3BucketLambdaProgram} eventJson =
          _lbdS3BucketLambdaProgram <$> (parseEither (`S3Event.parse` config) =<< eitherDecode (LBS.pack eventJson))

        parseLambdaEvent ApiLambda{_lbdApiMethodLambdaProgram} eventJson =
          _lbdApiMethodLambdaProgram <$> (parseEither (`ApiEvent.parse` config) =<< eitherDecode (LBS.pack eventJson))

        parseLambdaEvent CfCustomLambda{_lbdCFCustomLambdaProgram} eventJson =
          _lbdCFCustomLambdaProgram <$> (eitherDecode (LBS.pack eventJson) :: Either String CfEvent)

        lbdIO
          :: Lambda
          -> String
          -> IO ()
        lbdIO lbd eventJson =
          either
            (\err -> fail $ concat ["Could not parse event: ", show eventJson, ", error was: ", err])
            (`LIO.run` config)
            (parseLambdaEvent lbd eventJson)


