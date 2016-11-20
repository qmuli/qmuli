{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                hiding (view, (.=))
import           Control.Monad               (forM, void, (<=<))
import           Data.Aeson
import           Data.Aeson.Types            (typeMismatch)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.HashMap.Strict         as SHM

import           Network.AWS.DynamoDB        (AttributeValue, attributeValue,
                                              avS)

import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Delete, Get, Post),
                                              RequestBody (..), aeBody,
                                              aeParams, rpPath)
import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Config.Identifier        (DdbTableId)
import           Qi.Program.Config.Interface (ConfigProgram)
import qualified Qi.Program.Config.Interface as CI
import           Qi.Program.Lambda.Interface (LambdaProgram, deleteDdbRecord,
                                              getDdbRecord, output,
                                              putDdbRecord, scanDdbRecords)

import           Types


-- Used the two curl commands below to test-drive the two endpoints (substitute your unique api stage url first):
{-
export API="https://h5g9rrg8if.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things/mycup"
curl -v -X GET "$API/things/mycup"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/mycup"
-}


main :: IO ()
main =
  "apigwlambda" `withConfig` config

    where
      config :: ConfigProgram ()
      config = do
        api     <- CI.api "world"
        things  <- CI.apiRootResource "things" api
        thing   <- CI.apiChildResource "{thingId}" things

        thingsTable <- CI.ddbTable "things" (DdbAttrDef "Id" S) Nothing (DdbProvCap 1 1)

        void $ CI.apiMethodLambda
          "getAllThings"
          Get
          things
          $ getAllThingsLambda thingsTable

        void $ CI.apiMethodLambda
          "getThing"
          Get
          thing
          $ getThingLambda thingsTable

        void $ CI.apiMethodLambda
          "putThing"
          Post
          thing
          $ putThingLambda thingsTable

        void $ CI.apiMethodLambda
          "deleteThing"
          Delete
          thing
          $ deleteThingLambda thingsTable


      getAllThingsLambda
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      getAllThingsLambda ddbTableId event = do
        ddbRecords <- scanDdbRecords ddbTableId
        let thingsResult :: Result [Thing] = forM ddbRecords $ \ddbRecord ->
                      case SHM.lookup "Data" ddbRecord of
                        Just thingAttrs -> do
                          castFromDdbAttrs unDdbThing thingAttrs

                        Nothing ->
                          Error $ "Error: could not extract thing data, DDB record was: " ++ show ddbRecord


        case thingsResult of
          Success things ->
            output . LBS.toStrict $ encode things
          Error err ->
            output . BS.pack $ "Parsing error: " ++ show err



      getThingLambda
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      getThingLambda ddbTableId event = do
        withId event $ \tid -> do
          let keys = SHM.fromList [
                  ("Id", attributeValue & avS .~ Just tid)
                ]
          ddbRecord <- getDdbRecord ddbTableId keys
          case SHM.lookup "Data" ddbRecord of
            Just thingAttrs -> do
              case castFromDdbAttrs unDdbThing thingAttrs of
                Success (thing :: Thing)  ->
                  output . LBS.toStrict . encode $ thing
                Error err                 ->
                  output . BS.pack $ "Error: fromJson: " ++ err ++ ". Attrs were: " ++ show thingAttrs

            Nothing ->
              output . BS.pack $ "Error: could not extract thing data, DDB record was: " ++ show ddbRecord



      putThingLambda
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      putThingLambda ddbTableId event = do
        withId event $ \tid -> do
          withThingAttributes event $ \thingAttrs -> do
            let item = SHM.fromList [
                    ("Id",    attributeValue & avS .~ Just tid)
                  , ("Data",  thingAttrs)
                  ]

            putDdbRecord ddbTableId item
            output "successfully added item"

      deleteThingLambda
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      deleteThingLambda ddbTableId event = do
        withId event $ \tid -> do
          let keys = SHM.fromList [
                  ("Id", attributeValue & avS .~ Just tid)
                ]
          deleteDdbRecord ddbTableId keys
          output "successfully deleted item"



      withThingAttributes
        :: ApiEvent
        -> (AttributeValue -> LambdaProgram ())
        -> LambdaProgram ()
      withThingAttributes event f = case event^.aeBody of
        JsonBody jb -> case castToDdbAttrs DdbThing jb of
          Success thing -> f thing
          Error err     -> output . BS.pack $ "Error: fromJson: " ++ err ++ ". Json was: " ++ show jb
        unexpected  ->
          output . BS.pack $ "Unexpected request body: " ++ show unexpected


      withId event f = case SHM.lookup "thingId" $ event^.aeParams.rpPath of
        Just (String x) -> f x
        Just unexpected ->
          output $ BS.pack $ "unexpected path parameter: " ++ show unexpected
        Nothing ->
          output "expected path parameter 'thingId' was not found"


      castFromDdbAttrs
        :: (FromJSON a, ToJSON b, FromJSON c)
        => (a -> b)
        -> AttributeValue
        -> Result c
      castFromDdbAttrs ddbDeconstructor = fromJSON <=< fmap (toJSON . ddbDeconstructor) . fromJSON . toJSON

      castToDdbAttrs
        :: (FromJSON a, ToJSON b)
        => (a -> b)
        -> Value
        -> Result AttributeValue
      castToDdbAttrs ddbConstructor = fromJSON <=< fmap (toJSON . ddbConstructor) . fromJSON
