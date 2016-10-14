{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           System.Environment                   (getArgs)

import           Assets                               (config)
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.CF                         (render)
import           Qi.Config.Identifier
import qualified Qi.Program.Config.Interpreters.Build as CB
import qualified Qi.Program.Lambda.Interpreters.IO    as LIO


main :: IO ()
main = do

  [cmd] <- getArgs
  case cmd of
    "cf" -> do
      LBS.putStr configJson

    "lbd" -> do
      lbdIO


    x -> putStrLn $ "unexpected command: '" ++ x ++ "'"

  where
    configuration = snd . (`runState` def) $ CB.interpret config

    configJson = render configuration

    lbdIO = LIO.interpret $ (lbd ^. lbdS3BucketLambdaProgram) dummyS3Event
      where
        dummyS3Event = S3Event $ S3Object (S3BucketIdentifier 123) (S3Key "myfile")
        lbd = head . SHM.elems $ configuration ^. lbdConfig . lcLambdas






