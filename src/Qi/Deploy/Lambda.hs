{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Deploy.Lambda (deploy) where

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           Prelude                       hiding (FilePath, log)
import           System.Environment.Executable (splitExecutablePath)
import           Turtle                        (FilePath, fromString, liftIO,
                                                sh, toText)

import           Qi.Deploy.Build
import qualified Qi.Deploy.S3                  as S3


toTextIgnore :: FilePath -> T.Text
toTextIgnore x = case toText x of
  Right s -> s
  Left _  -> ""

deploy
  :: T.Text
  -> IO ()
  -- Note: the executable built in the AWS environment using Docker will always be named 'lambda'
deploy appName = do
  (_, execFilename) <- splitExecutablePath

  lambdaPackagePath <- fromString <$> build (buildConfig "." execFilename)
  sh . liftIO . uploadToS3 . T.unpack $ toTextIgnore lambdaPackagePath

  where
      uploadToS3
        :: String
        -> IO ()
      uploadToS3 path =
        S3.upload appName "lambda.zip" =<< LBS.readFile path

