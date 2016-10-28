{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.S3 (createBucket, upload) where

import           Control.Lens
import           Control.Monad.Trans.AWS (runAWST, send)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text)
import           Network.AWS             hiding (send)
import qualified Network.AWS.S3          as A
import           System.IO               (stdout)

import           Qi.Amazonka             (runAmazonka)


createBucket
  :: Text
  -> IO ()
createBucket appName = do
  runAmazonka $ do
    _ <- send $ A.createBucket (A.BucketName appName)
    return ()


upload
  :: Text
  -> Text
  -> LBS.ByteString
  -> IO ()
upload appName filename json = do
  runAmazonka $ do
    _ <- send $ A.putObject (A.BucketName appName) (A.ObjectKey filename) $ toBody json
    return ()

