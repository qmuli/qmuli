{-# LANGUAGE OverloadedStrings #-}

module Main where

{- import qualified Data.HashMap.Strict as SHM -}
import           Test.Tasty
import           Test.Tasty.Hspec
{- import           Text.Heredoc -}
{- import           Qi.Config.AWS.CF -}

import qualified Config.ApiGw.Simple              as ApiGw.Simple
import qualified Config.ApiGw.UserPoolsAuthorizer as ApiGw.UserPoolsAuthorizer
import qualified Config.DDB.Simple                as DDB.Simple
import qualified Config.DDB.Stream                as DDB.Stream
import qualified Config.Identifier                as Id
import qualified Config.Template                  as Template
import           Protolude

main :: IO ()
main = do
  tree <- sequence specs
  defaultMain $ testGroup "Qmuli" tree

  where
    specs = [
        testSpec "Template"    Template.spec
      , testSpec "Identifier"  Id.spec
      , testSpec "ApiGw Simple"               ApiGw.Simple.spec
      , testSpec "ApiGw UserPools Authorizer" ApiGw.UserPoolsAuthorizer.spec
      , testSpec "DDB Simple"               DDB.Simple.spec
      , testSpec "DDB Stream"               DDB.Stream.spec
      ]

{- decode json `shouldBe` Just createEvent -}

  {- where -}
    {- createEvent = CfEventCreate { -}
        {- _cfeResponseURL = "http://somewhere.com/something" -}
      {- , _cfeStackId = "myStackId" -}
      {- , _cfeRequestId = "myRequestId" -}
      {- , _cfeResourceType = "myResourceType" -}
      {- , _cfeLogicalResourceId = "myLogicalResourceId" -}
      {- , _cfeResourceProperties = SHM.empty -}
      {- } -}

    {- json = [there|./tests/json/cf-create-event.json/|] -}


