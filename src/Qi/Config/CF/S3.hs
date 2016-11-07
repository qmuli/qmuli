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
    toS3BucketRes s3@S3Bucket{_s3bName, _s3bEventConfigs} = (
      resource name $
        BucketProperties $
        bucket
        & bBucketName ?~ (Literal $ _s3bName `namePrefixWith` config)
        & bNotificationConfiguration ?~ lbdConfigs
      )
      & dependsOn ?~ reqs

      where
        name = getS3BucketCFResourceName s3

        reqs =
          map (\lec -> getLambdaResourceNameFromId (lec ^. lbdId) config ) _s3bEventConfigs


        lbdConfigs = s3NotificationConfiguration
          & sncLambdaConfigurations ?~ (map lbdC _s3bEventConfigs)

        lbdC S3EventConfig{_event, _lbdId} = s3NotificationConfigurationLambdaConfiguration
          (Literal . T.pack $ show _event)
          (GetAtt (getLambdaResourceNameFromId _lbdId config) "Arn")
