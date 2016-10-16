{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           System.Environment                   (getArgs)

import           Assets                               (config)
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event               as S3Event (parse)
import           Qi.Config.CF                         (render)
import           Qi.Config.Identifier
import qualified Qi.Program.Config.Interpreters.Build as CB
import           Qi.Program.Lambda.Interface          (LambdaProgram)
import qualified Qi.Program.Lambda.Interpreters.IO    as LIO


appName = "qmuli"


main :: IO ()
main = do

  args <- getArgs
  case args of
    [] -> do
      putStrLn "Unexpected arguments. <print usage>"

    "cf":_ -> do
      LBS.putStr configJson

    "lbd":[] -> do
      putStrLn "Please specify lambda name"

    "lbd":lbdName:event:_ -> do
      case SHM.lookup (T.pack lbdName) lbdIOMap of
        Nothing ->
          putStrLn $ "No lambda with name '" ++ lbdName ++ "' was found"
        Just lbdIO ->
          lbdIO event


    cmd:_ -> putStrLn $ "Unexpected command: '" ++ cmd ++ "'"

  where
    configuration = snd . (`runState` def{_namePrefix = appName}) $ CB.interpret config

    configJson = render configuration


    lbdIOMap = SHM.fromList $ map toLbdIOPair lbds

      where
        toLbdIOPair :: Lambda -> (Text, String -> IO ())
        toLbdIOPair S3BucketLambda{_lbdName, _lbdS3BucketLambdaProgram} = (_lbdName, lbdIO)
          where
            lbdIO :: String -> IO ()
            lbdIO eventJson = do
              case parseEither (`S3Event.parse` configuration) =<< eitherDecode (LBS.pack eventJson) of
                Right s3Event ->
                  LIO.run (_lbdS3BucketLambdaProgram s3Event) configuration
                Left err ->
                  fail $ "Could not parse event: " ++ show eventJson


        toLbdIOPair lbd = error $ "unsupported lambda constructor: " ++ show lbd

        lbds = SHM.elems $ configuration ^. lbdConfig . lcLambdas



