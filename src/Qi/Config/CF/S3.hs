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
import qualified Qi.Config.AWS.Lambda.Accessors as Lambda
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Accessors     as S3


toResources config = Resources . map toS3BucketRes $ S3.getAll config
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
        resName = S3.getLogicalName bucket
        bucketName = S3.getPhysicalName bucket config

        reqs = concat $
          map (\lec ->  let
                          lbd = Lambda.getById (lec^.lbdId) config
                        in
                        [ Lambda.getPermissionLogicalName lbd
                        , Lambda.getLogicalName lbd
                        ]) _s3bEventConfigs


        lbdConfigs = s3BucketNotificationConfiguration
          & sbncLambdaConfigurations ?~ (map lbdC _s3bEventConfigs)

        lbdC S3EventConfig{_event, _lbdId} =
          s3BucketLambdaConfiguration
            (Literal . T.pack $ show _event)
            (GetAtt (Lambda.getLogicalNameFromId _lbdId config) "Arn")
