{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.Lambda (deploy) where

import           Control.Lens                  hiding (view)
import           Control.Monad.Trans.AWS       (AWST, runAWST, send)
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           Network.AWS                   hiding (send)
import qualified Network.AWS.S3                as A
import           Prelude                       hiding (log)
import           System.Environment.Executable (getExecutablePath)
import           System.IO                     (stdout)
import           Turtle                        hiding (stdout)

import           Qi.Deploy.JS                  (js)


log = liftIO . echo

toTextIgnore x = case toText x of
  Right s -> s
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
      output "index.js" $ pure js

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
    uploadToS3 path = do
      env <- newEnv NorthVirginia Discover
      logger <- newLogger Debug stdout
      lambdaPackage <- LBS.readFile path
      runResourceT . runAWST (env & envLogger .~ logger) $ do
        r <- send $ A.putObject (A.BucketName appName) (A.ObjectKey "lambda.zip") $ toBody lambdaPackage
        return ()

