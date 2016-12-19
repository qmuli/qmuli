{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.S3.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdName, lcLambdas)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier


getS3BucketLogicalName
  :: S3Bucket
  -> Text
getS3BucketLogicalName bucket =  T.concat [bucket ^. s3bName, "S3Bucket"]

getFullBucketName
  :: S3Bucket
  -> Config
  -> Text
getFullBucketName bucket config =
  (bucket^.s3bName) `dotNamePrefixWith` config

getS3BucketById
  :: S3BucketId
  -> Config
  -> S3Bucket
getS3BucketById bid config =
  case SHM.lookup bid bucketMap of
    Just bucket -> bucket
    Nothing  -> error $ "Could not reference s3 bucket with id: " ++ show bid
  where
    bucketMap = config^.s3Config.s3Buckets.s3idxIdToS3Bucket

getS3BucketIdByName
  :: Text
  -> Config
  -> S3BucketId
getS3BucketIdByName bucketName config =
  case SHM.lookup bucketName bucketNameToIdMap of
    Just bid -> bid
    Nothing  -> error $ "Could not find s3 bucket id with name: " ++ show bucketName
  where
    bucketNameToIdMap = config ^. s3Config . s3Buckets . s3idxNameToId


getAllBuckets
  :: Config
  -> [S3Bucket]
getAllBuckets config = SHM.elems $ config ^. s3Config . s3Buckets . s3idxIdToS3Bucket

