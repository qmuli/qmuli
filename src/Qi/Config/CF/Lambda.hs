{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.Lambda (toResources) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.Lambda.Accessors
import qualified Qi.Config.CF.Role              as Role


toResources config = foldMap toAllLambdaResources lambdas
  where
    lambdas = getAllLambdas config

    toAllLambdaResources :: Lambda -> Resources
    toAllLambdaResources lbd = Resources $ [lambdaPermissionCFResource, lambdaCFResource]

    lbdResName = getLambdaResourceName lbd
    lbdPermResName = getLambdaPermissionResourceName lbd

    lambdaPermissionCFResource =
      resource lbdPermResName $
        LambdaPermissionProperties $
        lambdaPermission
          "lambda:*"
          (GetAtt lbdResName "Arn")
          principal
      where
        principal = case lbd of
          S3BucketLambda{}  -> "s3.amazonaws.com"
          ApiLambda{}       -> "apigateway.amazonaws.com"

    lambdaCFResource = (
      resource lbdResName $
        LambdaFunctionProperties $
        lambdaFunction
          lbdCode
          "index.handler"
          (GetAtt Role.lambdaBasicExecutionIAMRoleResourceName "Arn")
          "nodejs4.3"
        & lfFunctionName ?~ (Literal $ (lbd ^. lbdName) `namePrefixWith` config)
        & lfMemorySize ?~ Literal 1536
        & lfTimeout ?~ Literal 90
      )
      & dependsOn ?~ [ 
          Role.lambdaBasicExecutionIAMRoleResourceName
        , lbdPermResName
        ]

      where
        lbdCode :: LambdaFunctionCode
        lbdCode = lambdaFunctionCode
          & lfcS3Bucket ?~ lambdaS3Bucket
          & lfcS3Key    ?~ lambdaS3Object

        lambdaS3Bucket :: Val Text
        lambdaS3Bucket = Literal $ config ^. namePrefix

        lambdaS3Object :: Val Text
        lambdaS3Object = "lambda.zip"

