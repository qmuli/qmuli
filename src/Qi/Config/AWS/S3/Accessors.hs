{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.S3.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdName, lcLambdas)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier


getS3LambdaPermissionResourceName
  :: Lambda
  -> Text
getS3LambdaPermissionResourceName lbd =  T.concat [lbd ^. lbdName, "S3LambdaPermission"]

getS3BucketResourceName
  :: S3Bucket
  -> Text
getS3BucketResourceName bucket =  T.concat [bucket ^. s3bName, "S3Bucket"]





getS3BucketById
  :: S3BucketIdentifier
  -> Config
  -> S3Bucket
getS3BucketById bid = fromJust . SHM.lookup bid . s3bHm
  where
    s3bHm config = config ^. s3Config . s3Buckets . s3idxIdToS3Bucket

getS3BucketIdByName
  :: Text
  -> Config
  -> S3BucketIdentifier
getS3BucketIdByName bucketName = fromJust . SHM.lookup bucketName . nameToIdHm
  where
    nameToIdHm config = config ^. s3Config . s3Buckets . s3idxNameToId

getAllBuckets
  :: Config
  -> [S3Bucket]
getAllBuckets config = SHM.elems $ config ^. s3Config . s3Buckets . s3idxIdToS3Bucket
