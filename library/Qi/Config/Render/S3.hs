{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.Render.S3 (toResources) where

import           Control.Lens
import           Data.Aeson                     (Value (Array), object)
import           Protolude                      hiding (getAll)
import           Qi.Config.AWS                  (Config, getAll, getById,
                                                 getLogicalName,
                                                 getLogicalNameFromId,
                                                 getPhysicalName)
import qualified Qi.Config.AWS.Lambda.Accessors as L
import           Qi.Config.AWS.S3               (S3Bucket (..),
                                                 S3EventConfig (..), event,
                                                 lbdId, s3bEventConfigs)
import           Stratosphere                   (CannedACL (..), ResourceProperties (S3BucketProperties),
                                                 Resources (Resources),
                                                 Val (GetAtt, Literal))
import qualified Stratosphere                   as S (resource,
                                                      resourceDependsOn,
                                                      s3Bucket,
                                                      s3BucketLambdaConfiguration,
                                                      s3BucketNotificationConfiguration,
                                                      sbAccessControl,
                                                      sbBucketName,
                                                      sbNotificationConfiguration,
                                                      sbncLambdaConfigurations)


toResources
  :: Config
  -> Resources
toResources config = Resources . map toS3BucketRes $ getAll config
  where
    toS3BucketRes bucket@S3Bucket{_s3bEventConfigs} = (
      S.resource lname $
        S3BucketProperties $
          S.s3Bucket
          & S.sbBucketName ?~ (Literal bucketName)
          & S.sbAccessControl ?~ (Literal PublicReadWrite)
          & S.sbNotificationConfiguration ?~ lbdConfigs
      )
      & S.resourceDependsOn ?~ reqs

      where
        lname = getLogicalName config bucket
        bucketName = getPhysicalName config bucket
        eventConfigs = bucket ^. s3bEventConfigs

        reqs = concat $
          map (\lec ->
            let
              lbd = getById config (lec ^. lbdId)
            in
            [ L.getPermissionLogicalName config lbd
            , getLogicalName config lbd
            ]) eventConfigs


        lbdConfigs = S.s3BucketNotificationConfiguration
          & S.sbncLambdaConfigurations ?~ map lbdConfig eventConfigs

        lbdConfig s3EventConfig =
          S.s3BucketLambdaConfiguration
            (Literal . show $ s3EventConfig ^. event)
            (GetAtt (getLogicalNameFromId config $ s3EventConfig ^. lbdId) "Arn")
