{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.Render.Lambda (toResources) where

import           Protolude                      hiding (getAll)
import           Stratosphere

import           Qi.Config.AWS
{- import           Qi.Config.AWS.DDB -}
import           Qi.Config.AWS.Lambda           hiding (lbdName)
import           Qi.Config.AWS.Lambda.Accessors
import qualified Qi.Config.Render.Role          as Role


toResources :: Config -> Resources
toResources config = foldMap toAllLambdaResources $ getAll config
  where
    toAllLambdaResources :: Lambda -> Resources
    toAllLambdaResources lbd = Resources $ [lambdaPermissionResource, lambdaResource]

      where
        lbdName = getLogicalName config lbd
        lbdPermName = getPermissionLogicalName config lbd

        lambdaPermissionResource =
          resource (unLogicalName lbdPermName) $
            LambdaPermissionProperties $
            lambdaPermission
              "lambda:*"
              (GetAtt (unLogicalName lbdName) "Arn")
              principal
          where
            principal = case lbd of
              GenericLambda{}  -> "lambda.amazonaws.com"
              S3BucketLambda{} -> "s3.amazonaws.com"
              {- ApiLambda{}       -> "apigateway.amazonaws.com" -}
              CfCustomLambda{} -> "*" -- TODO: not sure whether we even need the permission for CF Custom Resource
              CwEventLambda{}  -> "events.amazonaws.com"
              {- DdbStreamLambda{} -> "dynamodb.amazonaws.com" -}

        lambdaResource = (
          resource (unLogicalName lbdName) $
            LambdaFunctionProperties $
            lambdaFunction
              lbdCode
              "index.handler"
              (GetAtt Role.lambdaBasicExecutionIAMRoleLogicalName "Arn")
              (Literal $ OtherRuntime "provided")
            & lfFunctionName  ?~ Literal (unPhysicalName $ getPhysicalName config lbd)
            & lfMemorySize    ?~ Literal memorySize
            & lfTimeout       ?~ Literal timeOut
          )

          where
            memorySize  = fromIntegral . fromEnum $ lbd ^. lbdProfile . lpMemorySize
            timeOut     = fromIntegral $ lbd ^. lbdProfile . lpTimeoutSeconds

            lbdCode :: LambdaFunctionCode
            lbdCode = lambdaFunctionCode
              & lfcS3Bucket ?~ lambdaS3Bucket
              & lfcS3Key    ?~ lambdaS3Object

            lambdaS3Bucket :: Val Text
            lambdaS3Bucket = Literal $ (config ^. namePrefix) <> ".app"

            lambdaS3Object :: Val Text
            lambdaS3Object = "lambda.zip"

