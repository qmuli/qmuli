{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.S3 (toResources) where

import           Control.Lens
import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import qualified Data.Text                      as T
import           Protolude                      hiding (getAll)
import           Qi.Config.AWS
import qualified Qi.Config.AWS.Lambda.Accessors as Lambda
import           Qi.Config.AWS.S3
import           Stratosphere                   (ResourceProperties (S3BucketProperties),
                                                 Resources (Resources),
                                                 Val (GetAtt, Literal),
                                                 resource, resourceDependsOn,
                                                 s3Bucket,
                                                 s3BucketLambdaConfiguration,
                                                 s3BucketNotificationConfiguration,
                                                 sbBucketName,
                                                 sbNotificationConfiguration,
                                                 sbncLambdaConfigurations)


toResources :: Config -> Resources
toResources config = Resources . map toS3BucketRes $ getAll config
  where
    toS3BucketRes bucket@S3Bucket{_s3bEventConfigs} = (
      resource lname $
        S3BucketProperties $
          s3Bucket
          & sbBucketName ?~ (Literal bucketName)
          & sbNotificationConfiguration ?~ lbdConfigs
      )
      & resourceDependsOn ?~ reqs

      where
        lname = getLogicalName config bucket
        bucketName = getPhysicalName config bucket

        reqs = concat $
          map (\lec ->  let
                          lbd = getById config (lec^.lbdId)
                        in
                        [ Lambda.getPermissionLogicalName config lbd
                        , getLogicalName config lbd
                        ]) _s3bEventConfigs


        lbdConfigs = s3BucketNotificationConfiguration
          & sbncLambdaConfigurations ?~ (map lbdC _s3bEventConfigs)

        lbdC S3EventConfig{_event, _lbdId} =
          s3BucketLambdaConfiguration
            (Literal . T.pack $ show _event)
            (GetAtt (getLogicalNameFromId config _lbdId) "Arn")
