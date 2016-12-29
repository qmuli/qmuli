{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.Role (
    toResources
  , lambdaBasicExecutionIAMRoleLogicalName
  ) where

import           Data.Aeson    (Value (Array), object)
import           Data.Text     (Text)
import           Stratosphere  hiding (name)

import           Qi.Config.AWS


lambdaBasicExecutionIAMRoleLogicalName :: Text
lambdaBasicExecutionIAMRoleLogicalName = "lambdaBasicExecutionIAMRole"

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
          , "s3:PutObject"
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
          , "cognito-idp:CreateUserPoolClient"

          , "cognito-identity:CreateIdentityPool"
          , "cognito-identity:DeleteIdentityPool"
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
