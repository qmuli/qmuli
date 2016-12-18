{-# LANGUAGE OverloadedStrings #-}

module Main where

{- import qualified Data.HashMap.Strict as SHM -}
import           Test.Tasty
import           Test.Tasty.Hspec
{- import           Text.Heredoc -}
{- import           Qi.Config.AWS.CF -}

import qualified Config.Api        as Api
import qualified Config.Identifier as Id

main :: IO ()
main = do
  tree <- sequence specs
  defaultMain $ testGroup "Qmuli" tree

  where
    specs = [
        testSpec "Api"         Api.spec
      , testSpec "Identifier"  Id.spec
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


