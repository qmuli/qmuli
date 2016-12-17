{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.HashMap.Strict as SHM
import qualified Test.Tasty
import           Test.Tasty.Hspec
{- import           Text.Heredoc -}

import           Qi.Config.AWS.CF

import qualified Config.Api          as Api

main :: IO ()
main = do
  test <- testSpec "qmuli" Api.spec
  Test.Tasty.defaultMain test

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


