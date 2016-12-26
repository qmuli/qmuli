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


getLogicalName
  :: S3Bucket
  -> Text
getLogicalName bucket =  T.concat [bucket ^. s3bName, "S3Bucket"]

getPhysicalName
  :: S3Bucket
  -> Config
  -> Text
getPhysicalName bucket config =
  (bucket^.s3bName) `dotNamePrefixWith` config

getById
  :: S3BucketId
  -> Config
  -> S3Bucket
getById bid config =
  case SHM.lookup bid bucketMap of
    Just bucket -> bucket
    Nothing  -> error $ "Could not reference s3 bucket with id: " ++ show bid
  where
    bucketMap = config^.s3Config.s3Buckets.s3idxIdToS3Bucket

getIdByName
  :: Text
  -> Config
  -> S3BucketId
getIdByName bucketName config =
  case SHM.lookup bucketName bucketNameToIdMap of
    Just bid -> bid
    Nothing  -> error $ "Could not find s3 bucket id with name: " ++ show bucketName
  where
    bucketNameToIdMap = config ^. s3Config . s3Buckets . s3idxNameToId

getAll
  :: Config
  -> [S3Bucket]
getAll config = SHM.elems $ config ^. s3Config . s3Buckets . s3idxIdToS3Bucket

