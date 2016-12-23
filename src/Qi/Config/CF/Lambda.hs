{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.Lambda (toResources) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.Lambda.Accessors
import qualified Qi.Config.CF.Role              as Role

toResources config = foldMap toAllLambdaResources $ getAllLambdas config
  where
    toAllLambdaResources :: Lambda -> Resources
    toAllLambdaResources lbd = Resources $ [lambdaPermissionResource, lambdaResource]

      where
        lbdLName = getLambdaLogicalName lbd
        lbdPermLName = getLambdaPermissionLogicalName lbd

        lambdaPermissionResource =
          resource lbdPermLName $
            LambdaPermissionProperties $
            lambdaPermission
              "lambda:*"
              (GetAtt lbdLName "Arn")
              principal
          where
            principal = case lbd of
              S3BucketLambda{} -> "s3.amazonaws.com"
              ApiLambda{}      -> "apigateway.amazonaws.com"
              CfCustomLambda{} -> "*" -- TODO: not sure whether we even need the permission for CF Custom Resource

        lambdaResource = (
          resource lbdLName $
            LambdaFunctionProperties $
            lambdaFunction
              lbdCode
              "index.handler"
              (GetAtt Role.lambdaBasicExecutionIAMRoleLogicalName "Arn")
              (Literal NodeJS43)
            & lfFunctionName ?~ (Literal lambdaName)
            & lfMemorySize ?~ Literal memorySize
            & lfTimeout ?~ Literal timeOut
          )

          where
            lambdaName = getFullLambdaName lbd config

            memorySize = fromIntegral . fromEnum $ lbd^.lbdProfile.lpMemorySize
            timeOut = fromIntegral $ lbd^.lbdProfile.lpTimeoutSeconds

            lbdCode :: LambdaFunctionCode
            lbdCode = lambdaFunctionCode
              & lfcS3Bucket ?~ lambdaS3Bucket
              & lfcS3Key    ?~ lambdaS3Object

            lambdaS3Bucket :: Val Text
            lambdaS3Bucket = Literal $ config ^. namePrefix

            lambdaS3Object :: Val Text
            lambdaS3Object = "lambda.zip"

