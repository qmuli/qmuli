{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.S3 (deploy) where

import           Control.Lens
import           Control.Monad.Trans.AWS (AWST, runAWST, send)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.AWS             hiding (send)
import qualified Network.AWS.S3          as A
import           System.IO               (stdout)


deploy
  :: Text
  -> IO ()
deploy appName = do
  env <- newEnv NorthVirginia Discover
  logger <- newLogger Debug stdout
  runResourceT . runAWST (env & envLogger .~ logger) $ do
    r <- send $ A.createBucket (A.BucketName appName)
    return ()


