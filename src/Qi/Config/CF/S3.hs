{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.S3 (toResources) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors


toResources config = Resources . map toS3BucketRes $ getAllBuckets config
  where
    toS3BucketRes s3Bucket@S3Bucket{_s3bEventConfigs} = (
      resource resName $
        BucketProperties $
        bucket
        & bBucketName ?~ (Literal bucketName)
        & bNotificationConfiguration ?~ lbdConfigs
      )
      & dependsOn ?~ reqs

      where
        resName = getS3BucketCFResourceName s3Bucket
        bucketName = getFullBucketName s3Bucket config

        reqs =
          map (\lec -> getLambdaCFResourceNameFromId (lec ^. lbdId) config ) _s3bEventConfigs


        lbdConfigs = s3NotificationConfiguration
          & sncLambdaConfigurations ?~ (map lbdC _s3bEventConfigs)

        lbdC S3EventConfig{_event, _lbdId} = s3NotificationConfigurationLambdaConfiguration
          (Literal . T.pack $ show _event)
          (GetAtt (getLambdaCFResourceNameFromId _lbdId config) "Arn")
