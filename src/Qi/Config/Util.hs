{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.Util where

import           Control.Lens
import           Data.Default         (Default, def)
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdName, lcLambdas)
import           Qi.Config.AWS.S3     (S3Bucket, S3Config, bucketName)
import           Qi.Config.Identifier


getLambdaById
  :: LambdaIdentifier
  -> Config
  -> Lambda
getLambdaById lid = fromJust . SHM.lookup lid . lbdHm
  where
    lbdHm config = config ^. lbdConfig . lcLambdas

getLambdaResourceNameFromId
  :: LambdaIdentifier
  -> Config
  -> Text
getLambdaResourceNameFromId lbdId config =
  getLambdaResourceName $ getLambdaById lbdId config

getLambdaResourceName
  :: Lambda
  -> Text
getLambdaResourceName lbd =  T.concat [lbd ^. lbdName, "Lambda"]

getS3LambdaPermissionResourceName
  :: Lambda
  -> Text
getS3LambdaPermissionResourceName lbd =  T.concat [lbd ^. lbdName, "S3LambdaPermission"]

getS3BucketResourceName
  :: S3Bucket
  -> Text
getS3BucketResourceName bucket =  T.concat [bucket ^. bucketName, "S3Bucket"]
