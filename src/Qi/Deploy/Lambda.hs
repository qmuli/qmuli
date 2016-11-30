{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Deploy.Lambda (deploy) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Prelude              hiding (FilePath, log)
import           Qi.Deploy.Build
import qualified Qi.Deploy.S3         as S3
import           Turtle               (FilePath, fromString, liftIO, sh, toText)


toTextIgnore :: FilePath -> T.Text
toTextIgnore x = case toText x of
  Right s -> s
  Left _  -> ""

deploy
  :: T.Text
  -> T.Text
  -> IO ()
deploy buildName appName = fromString <$> build (Build "." (T.unpack buildName)) >>=
  \ lambdaPackagePath -> sh $ liftIO . uploadToS3 . T.unpack $ toTextIgnore lambdaPackagePath
  where
      uploadToS3
        :: String
        -> IO ()
      uploadToS3 path =
        S3.upload appName "lambda.zip" =<< LBS.readFile path

