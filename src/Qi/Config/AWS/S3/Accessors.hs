{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.S3.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier


getIdByName
  :: Config
  -> Text
  -> S3BucketId
getIdByName config bucketName =
  case SHM.lookup bucketName bucketNameToIdMap of
    Just bid -> bid
    Nothing  -> error $ "Could not find s3 bucket id with name: " ++ show bucketName
  where
    bucketNameToIdMap = config ^. s3Config . s3Buckets . s3idxNameToId

