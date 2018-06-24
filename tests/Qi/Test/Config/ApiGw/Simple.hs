{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.ApiGw.Simple where

import           Control.Lens
import           Control.Monad               (void)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty    (encodePretty)
import           Data.Aeson.Lens             (key, nth)
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Default                (def)
import           Qi.Config.AWS.ApiGw         (ApiVerb (Get))
import           Qi.Program.Config.Interface (ConfigProgram, api,
                                              apiMethodLambda, apiResource)
import           Qi.Program.Lambda.Interface (ApiLambdaProgram)
import           Test.Tasty.Hspec

import           Config                      (getConfig, getOutputs,
                                              getResources, getTemplate)
import           Protolude
import           Util


configProgram :: ConfigProgram ()
configProgram =
  api "world" >>= \world ->
    apiResource "things" world >>= \things ->
      void $ apiMethodLambda "dummyLambda" Get
              things def dummyLambda def

dummyLambda
  :: ApiLambdaProgram
dummyLambda _ = undefined





spec :: Spec
spec = describe "Template" $ do
    let template = getTemplate $ getConfig configProgram
        expectedApiDeploymentLogicalName  = "worldApiDeployment"
        expectedApiLogicalName            = "worldApi"
        expectedApiResourceLogicalName    = "thingsApiResource"
        expectedApiMethodLogicalName      = "thingsGet"
        expectedCorsMethodLogicalName     = "thingsOptions"
        expectedLambdaLogicalName         = "dummyLambdaLambda"

    it "saves test template" $
      LBS.writeFile "tests/artifacts/apigw_simple_test_template.json" $ encodePretty template

    context "Outputs" $ do
      let outputs = getOutputs template

      it "has the Api URL" $
        outputs `shouldContainKey` "worldApiURL"


    context "Resources" $ do
      let resources = getResources template

-- Api
------
      it "has the expected Api resource under the correct logical name" $
        resources `shouldContainKey` expectedApiLogicalName

      context "Api" $ do
        let resource = getValueUnderKey expectedApiLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::ApiGateway::RestApi")

-- ApiResource
--------------
      it "has the expected ApiResource resource under the correct logical name" $
        resources `shouldContainKey` expectedApiResourceLogicalName

      context "ApiResource" $ do
        let resource = getValueUnderKey expectedApiResourceLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::ApiGateway::Resource")


-- ApiMethod
------------
      it "has the expected ApiMethod resource under the correct logical name" $
        resources `shouldContainKey` expectedApiMethodLogicalName

      context "ApiMethod" $ do
        let resource = getValueUnderKey expectedApiMethodLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::ApiGateway::Method")

-- DependsOn
        context "DependsOn" $ do
          let (Array dependencies) = getValueUnderKey "DependsOn" resource

          it "should contain expected Lambda permission" $
            dependencies `shouldBe` [
                "dummyLambdaLambda"
              , "dummyLambdaLambdaPermission"
              ]

-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies Get method" $
            properties `shouldContainKVPair` ("HttpMethod", String "GET")

          it "specifies correct Api resource reference" $
            properties `shouldContainKVPair` ("RestApiId", ref expectedApiLogicalName)

          it "specifies correct ApiResource resource reference" $
            properties `shouldContainKVPair` ("ResourceId", ref expectedApiResourceLogicalName)

          it "specifies correct AuthorizationType" $
            properties `shouldContainKVPair` ("AuthorizationType", String "NONE")

          context "MethodResponses" $ do
            let (Array methodResponses) = getValueUnderKey "MethodResponses" properties

            it "contains responses for all status codes" $
              fmap (getValueUnderKey "StatusCode") methodResponses `shouldBe` ["200", "400", "404", "500"]

-- Integration
          context "Integration" $ do
            let integration = getValueUnderKey "Integration" properties

            it "specifies correct Type" $
              integration `shouldContainKVPair` ("Type", String "AWS")

            it "specifies correct IntegrationHttpMethod - always POST" $
              integration `shouldContainKVPair` ("IntegrationHttpMethod", String "POST")

            it "contains RequestTemplates" $
              integration `shouldContainKey` "RequestTemplates"

            it "specifies correct PassthroughBehavior" $
              integration `shouldContainKVPair` ("PassthroughBehavior", String "WHEN_NO_TEMPLATES")

            context "IntegrationResponses" $ do
              let (Array integrationResponses) = getValueUnderKey "IntegrationResponses" integration

              it "contains responses for all status codes" $
                fmap (getValueUnderKey "StatusCode") integrationResponses `shouldBe` ["200", "400", "404", "500"]


            it "specifies correct Lambda logical name in Uri" $ do
              let (Just lbdLogicalName) = integration ^? key "Uri" . key "Fn::Join" . nth 1 . nth 3 . key "Fn::GetAtt" . nth 0
              lbdLogicalName `shouldBe` String expectedLambdaLogicalName


-- CORS Options Method
------------
      it "has the expected resource under the correct logical name" $
        resources `shouldContainKey` expectedCorsMethodLogicalName

      context "CORS Options Method" $ do
        let resource = getValueUnderKey expectedCorsMethodLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::ApiGateway::Method")

-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies Get method" $
            properties `shouldContainKVPair` ("HttpMethod", String "OPTIONS")

          it "specifies correct Api resource reference" $
            properties `shouldContainKVPair` ("RestApiId", ref expectedApiLogicalName)

          it "specifies correct ApiResource resource reference" $
            properties `shouldContainKVPair` ("ResourceId", ref expectedApiResourceLogicalName)

          it "specifies correct AuthorizationType" $
            properties `shouldContainKVPair` ("AuthorizationType", String "NONE")

-- Integration
          context "Integration" $ do
            let integration = getValueUnderKey "Integration" properties

            it "specifies correct Type" $
              integration `shouldContainKVPair` ("Type", String "MOCK")

            it "specifies correct IntegrationHttpMethod - always POST" $
              integration `shouldContainKVPair` ("IntegrationHttpMethod", String "POST")

            it "contains RequestTemplates" $
              integration `shouldContainKey` "RequestTemplates"

            it "specifies correct PassthroughBehavior" $
              integration `shouldContainKVPair` ("PassthroughBehavior", String "WHEN_NO_MATCH")

            context "IntegrationResponses" $ do
              let (Array integrationResponses) = getValueUnderKey "IntegrationResponses" integration

              it "contains responses for all status codes" $
                fmap (getValueUnderKey "StatusCode") integrationResponses `shouldBe` ["200"]



-- ApiDeployment
----------------
      it "has the expected ApiDeployment resource under the correct logical name" $
        resources `shouldContainKey` expectedApiDeploymentLogicalName

      context "ApiDeployment" $ do
        let resource = getValueUnderKey expectedApiDeploymentLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::ApiGateway::Deployment")

-- DependsOn
        context "DependsOn" $ do
          let (Array deps) = getValueUnderKey "DependsOn" resource

          it "should contain expected dependencies" $
            deps `shouldBe` fmap String [
                expectedApiLogicalName
              , expectedApiResourceLogicalName
              , expectedCorsMethodLogicalName
              , expectedApiMethodLogicalName
              ]

-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies StageName method" $
            properties `shouldContainKVPair` ("StageName", String "v1")

          it "specifies correct Api resource reference" $
            properties `shouldContainKVPair` ("RestApiId", ref expectedApiLogicalName)


-- Lambda
---------
      it "has the expected Lambda resource under the correct logical name" $
        resources `shouldContainKey` expectedLambdaLogicalName

      context "Lambda" $ do
        let resource = getValueUnderKey expectedLambdaLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::Lambda::Function")

-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies MemorySize" $
            properties `shouldContainKVPair` ("MemorySize", String "128")

          it "specifies Runtime" $
            properties `shouldContainKVPair` ("Runtime", String "nodejs8.10")

          it "specifies Role" $
            properties `shouldContainKVPair` ("Role", object [("Fn::GetAtt", Array [String "lambdaBasicExecutionIAMRole", String "Arn"])])

          it "specifies FunctionName" $
            properties `shouldContainKVPair` ("FunctionName", String "testApp_dummyLambda")

          it "specifies Code" $
            properties `shouldContainKVPair` ("Code", object [("S3Key", String "lambda.zip"), ("S3Bucket", String "testApp")])

          it "specifies Handler" $
            properties `shouldContainKVPair` ("Handler", String "index.handler")

          it "specifies Timeout" $
            properties `shouldContainKVPair` ("Timeout", String "30")


