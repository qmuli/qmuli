{-# LANGUAGE TemplateHaskell #-}

module Qi.Test.CF.CustomResource where

import           Data.Aeson
import           Data.Either
import           Data.FileEmbed
import qualified Data.HashMap.Strict as HM
import           Protolude
import           Qi.Config.AWS.CF
import           Qi.Config.CF.Types
import           Test.Tasty.Hspec


createCustomResourceEvent :: ByteString
createCustomResourceEvent = $(makeRelativeToProject "tests/json/cf-create-event.json" >>= embedFile)

spec :: Spec
spec = do

  describe "FromJSON instances" $ do

    describe "CfEvent" $ do

      it "can parse" $ eitherDecodeStrict createCustomResourceEvent `shouldBe` Right
        (CfEventCreate {
            _cfeServiceToken       = Arn "arn:aws:lambda:us-east-1:910653408535:function:cognitotestxx_cognitoPoolProvider"
          , _cfeResponseURL        = "https://cloudformation-custom-resource-response-useast1.s3.amazonaws.com/arn%3Aaws%3Acloudformation%3Aus-east-1%3A910653408535%3Astack/cognitotestxx/3b615fb0-7701-11e8-85ef-50fa5f2588d2%7CcognitoPoolProviderLambdaCustom%7C9616e4f8-9153-4bc6-bc02-53252002a8a3?AWSAccessKeyId=AKIAJGHTPRZJWHX6QVGA&Expires=1529778000&Signature=i3hplxObOjPQvet9UGeiCQZJ60A%3D"
          , _cfeStackId            = Arn "arn:aws:cloudformation:us-east-1:910653408535:stack/cognitotestxx/3b615fb0-7701-11e8-85ef-50fa5f2588d2"
          , _cfeRequestId          = "9616e4f8-9153-4bc6-bc02-53252002a8a3"
          , _cfeLogicalResourceId  = LogicalResourceId "cognitoPoolProviderLambdaCustom"
          , _cfeResourceType       = "AWS::CloudFormation::CustomResource"
          , _cfeResourceProperties = HM.fromList [("ServiceToken", String "arn:aws:lambda:us-east-1:910653408535:function:cognitotestxx_cognitoPoolProvider")]
          })



