{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF (render) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors


render
  :: Config
  -> LBS.ByteString
render config = encodeTemplate $
  template
    (toResources config)
    & description ?~ "Example"
    & formatVersion ?~ "2010-09-09"


lambdaBasicExecutionIAMRoleResourceName = "lambdaBasicExecutionIAMRole" :: Text


toResources
  :: Config
  -> Resources
toResources config@Config{_namePrefix} = mconcat [s3Resources, roleResources, lbdResources]
  where
    roleResources = Resources [lbdRoleRes]
      where
        lbdRoleRes = resource lambdaBasicExecutionIAMRoleResourceName $
          IAMRoleProperties $
          iamRole rolePolicyDocumentObject
          & iamrPolicies ?~ [ executePolicy ]
          & iamrRoleName ?~ (Literal $ "LambdaBasicExecutionRole" `namePrefixWith` config)
          & iamrPath ?~ "/"

        executePolicy =
          iamPolicies
          [ ("Version", "2012-10-17")
          , ("Statement", statement)
          ] $
          Literal $ "LambdaExecutionPolicy" `namePrefixWith` config


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

        toS3LambdaPermissionResource lbd@S3BucketLambda{_lbdName} =
          resource resourceName $
            LambdaPermissionProperties $
            lambdaPermission
              "lambda:*"
              (GetAtt (getLambdaResourceName lbd) "Arn")
              "s3.amazonaws.com"
          where
            resourceName = getS3LambdaPermissionResourceName lbd


        toLambdaResource lbd@S3BucketLambda{_lbdName} = (
          resource resourceName $
            LambdaFunctionProperties $
            lambdaFunction
              lbdCode
              "index.handler"
              (GetAtt lambdaBasicExecutionIAMRoleResourceName "Arn")
              "nodejs4.3"
            & lfFunctionName ?~ (Literal $ _lbdName `namePrefixWith` config)
          )
          & dependsOn ?~ [ lambdaBasicExecutionIAMRoleResourceName ]

          where
            resourceName = getLambdaResourceName lbd

            lbdCode :: LambdaFunctionCode
            lbdCode = lambdaFunctionCode
              & lfcZipFile ?~ code

            code :: Val Text
            code = ""
            {- code = "\ -}
                    {- \ process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT'] \ -}
                    {- \ var exec = require('child_process').exec; \ -}
                    {- \ exports.handler = function(event, context) { \ -}
                    {- \  var input = JSON.stringify(event['body-json']) \ -}
                    {- \    .replace(/\\\\/g, \"\\\\\\\\\") \ -}
                    {- \    .replace(/\\$/g, \"\\\\$\") \ -}
                    {- \    .replace(/'/g, \"\\\\'\") \ -}
                    {- \    .replace(/\"/g, \"\\\\\\\"\"); \ -}
                    {- \  console.log(\"input:\", input) \ -}
                    {- \  var child = exec('./" -}
                    {- ++ namePrefix ++ -}
                    {- " lbd " -}
                    {- ++ _lbdName ++ -}
                    {- "\"' + input + '\"', {maxBuffer: 1024 * 500}, function(error, stdout, stderr) { \ -}
                    {- \    console.log('stdout: ' + stdout); \ -}
                    {- \    console.log('stderr: ' + stderr); \ -}
                    {- \    if (error !== null) { console.log('exec error: ' + error); } \ -}
                    {- \    context.done(null, JSON.parse(stdout)); \ -}
                    {- \  }); \ -}
                    {- \ } \ -}
                    {- \ " -}

    s3Resources = Resources . map toS3BucketRes $ getAllBuckets config
      where
        toS3BucketRes s3@S3Bucket{_s3bName, _s3bLbdEventConfigs} = (
          resource name $
            BucketProperties $
            bucket
            & bBucketName ?~ (Literal $ _s3bName `namePrefixWith` config)
            & bNotificationConfiguration ?~ lbdConfigs
          )
          & dependsOn ?~ reqs

          where
            name = getS3BucketResourceName s3

            reqs =
              map (\lec -> getLambdaResourceNameFromId (lec ^. lbdId) config ) _s3bLbdEventConfigs



            lbdConfigs = s3NotificationConfiguration
              & sncLambdaConfigurations ?~ (map lbdC _s3bLbdEventConfigs)

            lbdC LambdaEventConfig{_event, _lbdId} = s3NotificationConfigurationLambdaConfiguration
              (Literal . T.pack $ show _event)
              (GetAtt (getLambdaResourceNameFromId _lbdId config) "Arn")
