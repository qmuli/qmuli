{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Deploy.Lambda (deploy) where

import           Control.Lens                  hiding (view)
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           Network.AWS                   hiding (send)
import qualified Network.AWS.S3                as A
import           Prelude                       hiding (log)
import           Qi.Deploy.Build
import qualified Qi.Deploy.S3                  as S3
import           System.Environment.Executable (getExecutablePath)
import           System.IO                     (stdout)
import           Text.Heredoc                  (there)
import           Turtle                        hiding (stdout)


log :: (MonadIO m) => T.Text -> m ()
log = liftIO . echo

toTextIgnore x = case toText x of
  Right s  -> s
  Left err -> ""

deploy
  :: Text
  -> IO ()
deploy appName = fromString <$> build (Build "." (T.unpack appName)) >>=
  \ lambdaPackagePath -> sh $ liftIO . uploadToS3 . T.unpack $ toTextIgnore lambdaPackagePath
  where
      uploadToS3
        :: String
        -> IO ()
      uploadToS3 path =
        S3.upload appName "lambda.zip" =<< LBS.readFile path

