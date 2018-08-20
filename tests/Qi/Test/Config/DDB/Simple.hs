{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Test.Config.DDB.Simple where
{-
import           Control.Lens
import           Control.Monad               (void)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty    (encodePretty)
import           Data.Aeson.Lens             (key, nth)
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Default                (def)
import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Program.Config.Interface (ConfigProgram, ddbTable)
import           Test.Tasty.Hspec

import           Config                      (getConfig, getOutputs,
                                              getResources, getTemplate)
import           Protolude
import           Util


configProgram :: ConfigProgram ()
configProgram = do
  table <- ddbTable "things" (DdbAttrDef "name" S) def

  return ()



spec :: Spec
spec = describe "Template" $ do
    let template = getTemplate $ getConfig configProgram
        expectedDdbTableLogicalName = "thingsDynamoDBTable"

    it "saves test template" $
      LBS.writeFile "tests/artifacts/ddb_simple_test_template.json" $ encodePretty template


    context "Resources" $ do
      let resources = getResources template


-- Table
------------
      it "has the expected Table resource under the correct logical name" $
        resources `shouldContainKey` expectedDdbTableLogicalName

      context "Table" $ do
        let resource = getValueUnderKey expectedDdbTableLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::DynamoDB::Table")


-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies correct primary key AttributeDefinitions" $
            properties `shouldContainKVPair` ("AttributeDefinitions", Array [object [
                ("AttributeType", String "S")
              , ("AttributeName", String "name")
              ]])

          it "specifies correct default provisioned capacity" $
            properties `shouldContainKVPair` ("ProvisionedThroughput", object [
                ("ReadCapacityUnits", String "2")
              , ("WriteCapacityUnits", String "2")
              ])

          it "specifies correct KeySchema" $
            properties `shouldContainKVPair` ("KeySchema", Array [object [
                ("KeyType", String "HASH")
              , ("AttributeName", String "name")
              ]])

          it "specifies correct physical name" $
            properties `shouldContainKVPair` ("TableName", String "testApp_things")


-}
