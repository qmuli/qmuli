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
import           System.Environment.Executable (getExecutablePath)
import           System.IO                     (stdout)
import           Text.Heredoc                  (there)
import           Turtle                        hiding (stdout)

import qualified Qi.Deploy.S3                  as S3


log = liftIO . echo

toTextIgnore x = case toText x of
  Right s  -> s
  Left err -> ""

deploy
  :: Text
  -> IO ()
deploy appName = sh $ do
  lambdaPackagePath <- createLambdaPackage
  liftIO . uploadToS3 . T.unpack $ toTextIgnore lambdaPackagePath

  where
    createLambdaPackage = do
      mktree ".deploy/lambda"
      cd ".deploy/lambda"

      -- write the JS wrapper
      output "index.js" $ pure [there|./js/index.js|]

      -- copy the global executable to the delpoy destination
      execPath <- liftIO getExecutablePath
      cp (fromString execPath) . fromText $ T.concat ["./", appName]

      log "zipping package..."
      -- zip a package
      shell "zip package.zip *" empty

      path <- realpath "package.zip"
      cd "../.."

      return path

    uploadToS3
      :: String
      -> IO ()
    uploadToS3 path =
      S3.upload appName "lambda.zip" =<< LBS.readFile path

