{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.ApiGw.UserPoolsAuthorizer where

import           Control.Lens
import           Control.Monad                         (void)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty              (encodePretty)
import           Data.Aeson.Lens                       (key, nth)
import qualified Data.ByteString.Lazy.Char8            as LBS
import           Data.Default                          (def)
import           Qi.Config.AWS.ApiGw                   (ApiVerb (Get))
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ampAuthId)
import           Qi.Program.Config.Interface           (ConfigProgram, api,
                                                        apiAuthorizer,
                                                        apiMethodLambda,
                                                        apiResource,
                                                        customResource)
import           Qi.Program.Lambda.Interface           (ApiLambdaProgram)
import           Qi.Util.Cognito                       (cognitoPoolProviderLambda)
import           Test.Tasty.Hspec

import           Config                                (getConfig, getOutputs,
                                                        getResources,
                                                        getTemplate)
import           Protolude
import           Util


configProgram :: ConfigProgram ()
configProgram = do
  cognito <- customResource "cognitoPoolProvider"
    cognitoPoolProviderLambda def


  api "world" >>= \world -> do
    authId <- apiAuthorizer "myAuth" cognito world

    apiResource "things" world >>= \things ->
      void $ apiMethodLambda "dummyLambda" Get things
              (def & ampAuthId ?~ authId)
              dummyLambda def

dummyLambda :: ApiLambdaProgram
dummyLambda _ = undefined


spec :: Spec
spec = describe "Template" $ do
    let template = getTemplate $ getConfig configProgram
        expectedApiMethodLogicalName = "thingsGet"

    it "saves test template" $
      LBS.writeFile "tests/artifacts/apigw_userpool_authorizer_test_template.json" $ encodePretty template


    context "Resources" $ do
      let resources = getResources template

-- ApiMethod
------------
      context "ApiMethod" $ do
        let resource = getValueUnderKey expectedApiMethodLogicalName resources

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

          it "specifies correct AuthorizationType" $
            properties `shouldContainKVPair` ("AuthorizationType", String "COGNITO_USER_POOLS")



