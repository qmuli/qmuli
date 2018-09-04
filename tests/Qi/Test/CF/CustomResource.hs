{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.Test.CF.CustomResource where

import           Data.Aeson
import           Data.Either
import           Data.FileEmbed
import qualified Data.HashMap.Strict                  as HM
import           Protolude
import           Qi.AWS.CF
import           Qi.AWS.Cognito                       (IdPoolId (IdPoolId),
                                                       UserPoolId (UserPoolId))
import           Qi.AWS.Types
import           Qi.Config.AWS.CfCustomResource.Types
import           Test.Tasty.Hspec


createCustomResourceEvent :: ByteString
createCustomResourceEvent = $(makeRelativeToProject "tests/json/cf-create-event.json" >>= embedFile)

deleteCustomResourceEvent :: ByteString
deleteCustomResourceEvent = $(makeRelativeToProject "tests/json/cf-delete-event.json" >>= embedFile)


spec :: Spec
spec = do

  describe "FromJSON instances" $ do

    describe "CfCustomResourceEvent" $ do


      it "can parse " $ eitherDecodeStrict "\"CustomResourceId [ArnResourceId (Arn \\\"da\\\")]\"" `shouldBe` Right ( String $ show $ CustomResourceId [ArnResourceId $ Arn "da"] )


      it "can parse CfCustomResourceCreate" $ eitherDecodeStrict createCustomResourceEvent `shouldBe` Right
        (CfCustomResourceCreate {
            _cfeServiceToken       = Arn "arn:aws:lambda:us-east-1:910653408535:function:cognitotest_cognitoPoolProvider"
          , _cfeResponseURL        = "https://cloudformation-custom-resource-response-useast1.s3.amazonaws.com/arn%3Aaws%3Acloudformation%3Aus-east-1%3A910653408535%3Astack/cognitotest/3b615fb0-7701-11e8-85ef-50fa5f2588d2%7CcognitoPoolProviderLambdaCustom%7C9616e4f8-9153-4bc6-bc02-53252002a8a3?AWSAccessKeyId=AKIAJGHTPRZJWHX6QVGA&Expires=1529778000&Signature=i3hplxObOjPQvet9UGeiCQZJ60A%3D"
          , _cfeStackId            = Arn "arn:aws:cloudformation:us-east-1:910653408535:stack/cognitotest/3b615fb0-7701-11e8-85ef-50fa5f2588d2"
          , _cfeRequestId          = "9616e4f8-9153-4bc6-bc02-53252002a8a3"
          , _cfeLogicalResourceId  = LogicalResourceId "cognitoPoolProviderLambdaCustom"
          , _cfeResourceType       = "AWS::CloudFormation::CustomResource"
          , _cfeResourceProperties = [ ("ServiceToken", String "arn:aws:lambda:us-east-1:910653408535:function:cognitotest_cognitoPoolProvider") ]
          })

      it "can parse CfCustomResourceDelete" $ eitherDecodeStrict deleteCustomResourceEvent `shouldBe` Right
        (CfCustomResourceDelete {
            _cfeResponseURL        = "https://cloudformation-custom-resource-response-useast1.s3.amazonaws.com/arn%3Aaws%3Acloudformation%3Aus-east-1%3A910653408535%3Astack/cognitotest/3b615fb0-7701-11e8-85ef-50fa5f2588d2%7CcognitoPoolProviderLambdaCustom%7C9616e4f8-9153-4bc6-bc02-53252002a8a3?AWSAccessKeyId=AKIAJGHTPRZJWHX6QVGA&Expires=1529778000&Signature=i3hplxObOjPQvet9UGeiCQZJ60A%3D"
          , _cfeStackId            = Arn "arn:aws:cloudformation:us-east-1:910653408535:stack/cognitotest/3b615fb0-7701-11e8-85ef-50fa5f2588d2"
          , _cfeRequestId          = "9616e4f8-9153-4bc6-bc02-53252002a8a3"
          , _cfeResourceType       = "AWS::CloudFormation::CustomResource"
          , _cfeLogicalResourceId  = LogicalResourceId "cognitoPoolProviderLambdaCustom"
          , _cfePhysicalResourceId = CustomResourceId [ UserPoolIdResourceId $ UserPoolId "da"
                                                      , IdPoolIdResourceId $ IdPoolId "net" ]
          , _cfeResourceProperties = [ ("ServiceToken", String "arn:aws:lambda:us-east-1:910653408535:function:cognitotest_cognitoPoolProvider") ]
          })


