{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Char                            (isDigit, isLower)
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           System.Environment                   (getArgs)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.Lambda.Accessors       (getAllLambdas)
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event               as S3Event (parse)
import           Qi.Config.CF                         as CF
import           Qi.Config.Identifier
import qualified Qi.Deploy.Lambda                     as Lambda
import qualified Qi.Deploy.S3                         as S3
import           Qi.Program.Config.Interface          (ConfigProgram)
import qualified Qi.Program.Config.Interpreters.Build as CB
import           Qi.Program.Lambda.Interface          (LambdaProgram)
import qualified Qi.Program.Lambda.Interpreters.IO    as LIO


withConfig
  :: Text
  -> ConfigProgram ()
  -> IO ()
withConfig appName configProgram = do

  if invalid appName
    then
      putStrLn $ "Invalid qmulus name: '" ++ T.unpack appName ++ "', the name should only contain alphanumeric lower case characters"
    else do
      args <- getArgs
      case args of
        "deploy":[] -> do -- deploy CF template and the lambda package
          S3.createBucket appName
          S3.upload appName "cf.json" $ CF.render config
          Lambda.deploy appName

        -- execute the lambda on the event
        "lbd":lbdName:event:_ -> do
          case SHM.lookup (T.pack lbdName) lbdIOMap of
            Nothing ->
              putStrLn $ "No lambda with name '" ++ lbdName ++ "' was found"
            Just lbdIO ->
              lbdIO event


        _ -> putStrLn $ "Unexpected arguments: '" ++ show args ++ "'"

  where
    invalid = not . all ((||) <$> isLower <*> isDigit) . T.unpack

    config = snd . (`runState` def{_namePrefix = appName}) $ CB.interpret configProgram

    lbdIOMap = SHM.fromList $ map toLbdIOPair $ getAllLambdas config

      where
        toLbdIOPair :: Lambda -> (Text, String -> IO ())
        toLbdIOPair S3BucketLambda{_lbdName, _lbdS3BucketLambdaProgram} = (_lbdName, lbdIO)
          where
            lbdIO :: String -> IO ()
            lbdIO eventJson = do
              case parseEither (`S3Event.parse` config) =<< eitherDecode (LBS.pack eventJson) of
                Right s3Event ->
                  LIO.run (_lbdS3BucketLambdaProgram s3Event) config
                Left err ->
                  fail $ "Could not parse event: " ++ show eventJson

        toLbdIOPair lbd = error $ "unsupported lambda constructor: " ++ show lbd

