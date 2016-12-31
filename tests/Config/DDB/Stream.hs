{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.DDB.Stream where

import           Control.Lens
import           Control.Monad               (void)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty    (encodePretty)
import           Data.Aeson.Lens             (key, nth)
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Default                (def)
import           Test.Tasty.Hspec

import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Program.Config.Interface (ConfigProgram, ddbStreamLambda,
                                              ddbTable)
import           Qi.Program.Lambda.Interface (DdbStreamLambdaProgram)

import           Config                      (getConfig, getOutputs,
                                              getResources, getTemplate)
import           Util


configProgram :: ConfigProgram ()
configProgram = do
  table <- ddbTable "things" (DdbAttrDef "name" S) def
  void $ ddbStreamLambda "ddbStreamLambda" table handler def

handler
  :: DdbStreamLambdaProgram
handler _ = undefined


expectedLambdaPhysicalName = "testApp_ddbStreamLambda"
expectedDdbTableLogicalName = "thingsDynamoDBTable"
expectedDdbTableEventSourceMappingLogicalName = "thingsDynamoDBTableEventSourceMapping"

spec :: Spec
spec = describe "Template" $ do
    let template = getTemplate $ getConfig configProgram
    it "saves test template" $
      LBS.writeFile "tests/artifacts/ddb_stream_test_template.json" $ encodePretty template


    context "Resources" $ do
      let resources = getResources template


-- Table
------------
      context "Table" $ do
        let resource = getValueUnderKey expectedDdbTableLogicalName resources

-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies correct KeySchema" $
            properties `shouldContainKVPair` ("StreamSpecification", object [
                ("StreamViewType", String "NEW_AND_OLD_IMAGES")
              ])


-- EventSourceMapping
---------
      it "has the expected EventSourceMapping resource under the correct logical name" $
        resources `shouldContainKey` expectedDdbTableEventSourceMappingLogicalName

      context "EventSourceMapping" $ do
        let resource = getValueUnderKey expectedDdbTableEventSourceMappingLogicalName resources

        it "contains correct resource type" $
          resource `shouldContainKVPair` ("Type", String "AWS::Lambda::EventSourceMapping")

-- Properties
        context "Properties" $ do
          let properties = getValueUnderKey "Properties" resource

          it "specifies EventSourceArn" $
            properties `shouldContainKVPair` ("EventSourceArn", object [
                ("Fn::GetAtt", Array [String "thingsDynamoDBTable", String "StreamArn"])
              ])

          it "specifies FunctionName" $
            properties `shouldContainKVPair` ("FunctionName", String expectedLambdaPhysicalName)

          it "specifies StartingPosition" $
            properties `shouldContainKVPair` ("StartingPosition", String "LATEST")


