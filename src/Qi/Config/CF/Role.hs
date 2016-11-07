{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.Role (
    toResources
  , lambdaBasicExecutionIAMRoleResourceName
  ) where

import           Data.Aeson    (Value (Array), object)
import           Data.Text     (Text)
import           Stratosphere  hiding (name)

import           Qi.Config.AWS


lambdaBasicExecutionIAMRoleResourceName :: Text
lambdaBasicExecutionIAMRoleResourceName = "lambdaBasicExecutionIAMRole"

toResources
  :: Config
  -> Resources
toResources config = Resources [lbdRoleRes]
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
