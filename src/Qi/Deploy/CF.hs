{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.CF (deploy) where

import           Control.Lens               hiding (view)
import           Control.Monad.Trans.AWS    (AWST, runAWST, send)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import           Network.AWS                hiding (send)
import qualified Network.AWS.CloudFormation as A
import qualified Network.AWS.S3             as A
import           Prelude                    hiding (log)
import           System.IO                  (stdout)
import           Turtle                     hiding (stdout)


deploy
  :: Text
  -> LBS.ByteString
  -> IO ()
deploy appName json = do
  sh $ mktree ".deploy"
  uploadToS3 json

  where
    uploadToS3
      :: LBS.ByteString
      -> IO ()
    uploadToS3 json = do
      env <- newEnv NorthVirginia Discover
      logger <- newLogger Debug stdout
      runResourceT . runAWST (env & envLogger .~ logger) $ do
        r <- send $ A.putObject (A.BucketName appName) (A.ObjectKey "cf.json") $ toBody json
        return ()


