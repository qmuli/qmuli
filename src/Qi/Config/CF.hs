{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF (render) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors
import           Qi.Config.Identifier


render
  :: Text
  -> Config
  -> LBS.ByteString
render namePrefix config = encodeTemplate $
  template
    (toResources namePrefix config)
    & description ?~ "Example"
    & formatVersion ?~ "2010-09-09"


lambdaBasicExecutionIAMRoleResourceName = "lambdaBasicExecutionIAMRole"


toResources
  :: Text
  -> Config
  -> Resources
toResources namePrefix config = mconcat [s3Resources, roleResources, lbdResources]
  where
    roleResources = Resources [lbdRoleRes]
      where
        lbdRoleRes = resource lambdaBasicExecutionIAMRoleResourceName $
          IAMRoleProperties $
          iamRole rolePolicyDocumentObject
          & iamrPolicies ?~ [ executePolicy ]
          & iamrRoleName ?~ (Literal $ T.concat [namePrefix, "-", "LambdaBasicExecutionRole"])
          & iamrPath ?~ "/"

        executePolicy =
          iamPolicies
          [ ("Version", "2012-10-17")
          , ("Statement", statement)
          ] $
          Literal $ T.concat [namePrefix, "-", "LambdaExecutionPolicy"]


          where
            statement = object
              [ ("Effect", "Allow")
              , ("Action", actions)
              , ("Resource", "*")
              ]

            actions = Array
              [ "logs:CreateLogGroup"
              , "logs:CreateLogStream"
              , "logs:PutLogEvents"

              , "s3:PutObject"
              , "s3:GetObject"
              ]


        rolePolicyDocumentObject =
          [ ("Version", "2012-10-17")
          , ("Statement", statement)
          ]

          where
            statement = object
              [ ("Effect", "Allow")
              , ("Principal", principal)
              , ("Action", "sts:AssumeRole")
              ]

            principal = object
              [ ("Service", "lambda.amazonaws.com") ]


    lbdResources = foldMap toAllLambdaResources lambdas
      where

        lambdas = SHM.elems (config ^. lbdConfig . lcLambdas)

        toAllLambdaResources :: Lambda -> Resources
        toAllLambdaResources l = Resources $ [toS3LambdaPermissionResource l, toLambdaResource l]

        toS3LambdaPermissionResource lbd@S3BucketLambda{_lbdName} = resource name $
          LambdaPermissionProperties $
          lambdaPermission
            "lambda:*"
            (GetAtt (getLambdaResourceName lbd) "Arn")
            "s3.amazonaws.com"
          where
            name = getS3LambdaPermissionResourceName lbd


        toLambdaResource lbd@S3BucketLambda{_lbdName} = (
          resource name $
            LambdaFunctionProperties $
            lambdaFunction
              lbdCode
              "index.handler"
              (GetAtt lambdaBasicExecutionIAMRoleResourceName "Arn")
              "nodejs4.3"
            & lfFunctionName ?~ (Literal $ T.concat [namePrefix, "-", _lbdName])
          )
          & dependsOn ?~ [ lambdaBasicExecutionIAMRoleResourceName ]

          where
            name = getLambdaResourceName lbd

            lbdCode :: LambdaFunctionCode
            lbdCode = lambdaFunctionCode
              & lfcZipFile ?~ code

            code :: Val Text
            code = "\
            \ var AWS = require('aws-sdk'); \
            \ var s3 = new AWS.S3({apiVersion: '2006-03-01'}); \
            \ exports.handler = function(event, context, callback) { \
            \  console.log(JSON.stringify(event)); \
            \  var rec = event.Records[0]; \
            \  var bucket = rec.s3.bucket.name; \
            \  var key = rec.s3.object.key; \
            \  s3.copyObject({Bucket: \"qmuli-outgoing\", Key: key, CopySource: \"qmuli-incoming/\"+key}, function(err){ \
            \    callback(null, \"copied s3 object\"); \
            \  }); \
            \ } \
            \ "



    s3Resources = Resources . map toS3BucketRes $ getAllBuckets config
      where
        toS3BucketRes s3@S3Bucket{_s3bName, _s3bLbdEventConfigs} = (
          resource name $
            BucketProperties $
            bucket
            & bBucketName ?~ (Literal $ T.concat [namePrefix, "-", _s3bName])
            & bNotificationConfiguration ?~ lbdConfigs
          )
          & dependsOn ?~ reqs

          where
            name = getS3BucketResourceName s3

            reqs =
              map (\lec -> getLambdaResourceNameFromId (lec ^. lbdId) config ) _s3bLbdEventConfigs



            lbdConfigs = s3NotificationConfiguration
              & sncLambdaConfigurations ?~ (map lbdConfig _s3bLbdEventConfigs)

            lbdConfig LambdaEventConfig{_event, _lbdId} = s3NotificationConfigurationLambdaConfiguration
              (Literal . T.pack $ show _event)
              (GetAtt (getLambdaResourceNameFromId _lbdId config) "Arn")
