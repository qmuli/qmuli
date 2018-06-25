{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.Render.Role (
    toResources
  , lambdaBasicExecutionIAMRoleLogicalName
  ) where

import           Data.Aeson    (Value (Array), object)
import           Protolude
import           Stratosphere

import           Qi.Config.AWS


lambdaBasicExecutionIAMRoleLogicalName :: Text
lambdaBasicExecutionIAMRoleLogicalName = "lambdaBasicExecutionIAMRole"

-- authenticatedIAMRoleLogicalName :: Text
-- authenticatedIAMRoleLogicalName = "authenticatedIAMRole"


toResources
  :: Config
  -> Resources
toResources config = Resources [lbdRoleRes]
  where
    lbdRoleRes = resource lambdaBasicExecutionIAMRoleLogicalName $
      IAMRoleProperties $
      iamRole rolePolicyDocumentObject
      & iamrPolicies ?~ [ executePolicy ]
      & iamrRoleName ?~ (Literal $ "LambdaBasicExecutionRole" `underscoreNamePrefixWith` config)
      & iamrPath ?~ "/"

      where
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

        executePolicy =
          iamRolePolicy
          [ ("Version", "2012-10-17")
          , ("Statement", statement)
          ] $
          Literal $ "LambdaExecutionPolicy" `underscoreNamePrefixWith` config


          where
            statement = object
              [ ("Effect", "Allow")
              , ("Action", actions)
              , ("Resource", "*")
              ]

            actions = Array
              [ "logs:CreateLogGroup"
              , "logs:DescribeLogGroups"
              , "logs:CreateLogStream"
              , "logs:DescribeLogStreams"
              , "logs:PutLogEvents"

              , "s3:GetObject"
              , "s3:GetObjectAcl"
              , "s3:GetObjectTagging"
              , "s3:GetObjectVersion"
              , "s3:GetObjectVersionAcl"
              , "s3:GetObjectVersionTagging"
              , "s3:PutObject"
              , "s3:PutObjectAcl"
              , "s3:PutObjectTagging"
              , "s3:PutObjectVersionAcl"
              , "s3:PutObjectVersionTagging"
              , "s3:DeleteObject"
              , "s3:DeleteObjectTagging"
              , "s3:DeleteObjectVersion"
              , "s3:DeleteObjectVersionTagging"
              , "s3:RestoreObject"
              , "s3:CreateBucket"
              , "s3:DeleteBucket"
              , "s3:ListBucket"
              , "s3:ListBucketVersions"
              , "s3:ListAllMyBuckets"
              , "s3:ListBucketMultipartUploads"
              , "s3:ListMultipartUploadParts"
              , "s3:AbortMultipartUpload"

              , "dynamodb:Scan"
              , "dynamodb:Query"
              , "dynamodb:GetItem"
              , "dynamodb:PutItem"
              , "dynamodb:DeleteItem"
              , "dynamodb:GetRecords"
              , "dynamodb:GetShardIterator"
              , "dynamodb:DescribeStream"
              , "dynamodb:ListStreams"

              , "cognito-idp:CreateUserPool"
              , "cognito-idp:DeleteUserPool"
              , "cognito-idp:CreateUserPoolClient"

              , "cognito-identity:CreateIdentityPool"
              , "cognito-identity:DeleteIdentityPool"
              , "cognito-identity:SetIdentityPoolRoles"

              , "iam:CreateRole"
              , "iam:DeleteRole"
              , "iam:PassRole"
              , "iam:PutRolePolicy"
              , "iam:DeleteRolePolicy"

              , "lambda:InvokeFunction"

              , "lex:*"
              ]


