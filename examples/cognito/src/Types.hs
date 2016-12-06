{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import           Data.Aeson
import           Data.Aeson.Types (fieldLabelModifier, typeMismatch)
import           Data.Text        (Text)
import           GHC.Generics

{- { -}
   {- "Status" : "SUCCESS", -}
   {- "PhysicalResourceId" : "Tester1", -}
   {- "StackId" : "arn:aws:cloudformation:us-west-2:123456789012:stack/stack-name/guid", -}
   {- "RequestId" : "unique id for this create request", -}
   {- "LogicalResourceId" : "MySeleniumTester", -}
   {- "Data" : { -}
      {- "resultsPage" : "http://www.myexampledomain/test-results/guid", -}
      {- "lastUpdate" : "2012-11-14T03:30Z", -}
   {- } -}
{- }  -}

data CfStatus = CfSuccess | CfFailed
instance ToJSON CfStatus where
  toJSON CfSuccess = String "SUCCESS"
  toJSON CfFailed  = String "FAILED"

data Response = Response {
    rStatus             :: CfStatus
  , rReason             :: Text
  , rPhysicalResourceId :: Text
  , rStackId            :: Text
  , rRequestId          :: Text
  , rLogicalResourceId  :: Text
  , rData               :: Object
  } deriving Generic

instance ToJSON Response where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = drop 1 }
