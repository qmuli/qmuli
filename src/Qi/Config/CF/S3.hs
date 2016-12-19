{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.S3 (toResources) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (S3Bucket, name)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors


toResources config = Resources . map toS3BucketRes $ getAllBuckets config
  where
    toS3BucketRes bucket@S3Bucket{_s3bEventConfigs} = (
      resource resName $
        S3BucketProperties $
        s3Bucket
        & sbBucketName ?~ (Literal bucketName)
        & sbNotificationConfiguration ?~ lbdConfigs
      )
      & dependsOn ?~ reqs

      where
        resName = getS3BucketLogicalName bucket
        bucketName = getFullBucketName bucket config

        reqs =
          map (\lec -> getLambdaLogicalNameFromId (lec ^. lbdId) config ) _s3bEventConfigs


        lbdConfigs = s3BucketNotificationConfiguration
          & sbncLambdaConfigurations ?~ (map lbdC _s3bEventConfigs)

        lbdC S3EventConfig{_event, _lbdId} = s3BucketLambdaConfiguration
          (Literal . T.pack $ show _event)
          (GetAtt (getLambdaLogicalNameFromId _lbdId config) "Arn")
