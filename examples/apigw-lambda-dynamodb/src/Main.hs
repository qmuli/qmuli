{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                hiding (view, (.=))
import           Control.Monad               (forM, void, (<=<))
import           Data.Aeson
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
import           Util


-- Used the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://txyc43xz58.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things/mycup"
curl -v -X GET "$API/things/mycup"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"chair\", \"shape\": \"square\", \"size\": 10}" "$API/things/chair"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/mycup"
curl -v -X GET "$API/things"
-}


main :: IO ()
main =
  "apigwlambda" `withConfig` config

    where
      config :: ConfigProgram ()
      config = do
        thingsTable <- CI.ddbTable "things" (DdbAttrDef "Id" S) Nothing (DdbProvCap 1 1)

        CI.api "world" >>= \api ->
          CI.apiRootResource "things" api >>= \things -> do

            CI.apiMethodLambda "scanThings" Get
              things $ scan thingsTable


            CI.apiChildResource "{thingId}" things >>= \thing -> do

              CI.apiMethodLambda "getThing" Get
                thing $ get thingsTable

              CI.apiMethodLambda "putThing" Post
                thing $ put thingsTable

              CI.apiMethodLambda "deleteThing" Delete
                thing $ delete thingsTable

        return ()

      scan
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      scan ddbTableId event = do
        ddbRecords <- scanDdbRecords ddbTableId
        let thingsResult :: Result [Thing] = forM ddbRecords $ \ddbRecord ->
              case SHM.lookup "Data" ddbRecord of
                Just thingAttrs -> do
                  castFromDdbAttrs unDdbThing thingAttrs

                Nothing ->
                  Error $ "Error: could not extract thing data, DDB record was: " ++ show ddbRecord


        output $ case thingsResult of
          Success things ->
            LBS.toStrict $ encode things
          Error err ->
            BS.pack $ "Parsing error: " ++ show err



      get
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      get ddbTableId event = do
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



      put
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      put ddbTableId event = do
        withId event $ \tid -> do
          withThingAttributes event $ \thingAttrs -> do
            let item = SHM.fromList [
                    ("Id",    attributeValue & avS .~ Just tid)
                  , ("Data",  thingAttrs)
                  ]

            putDdbRecord ddbTableId item
            output "successfully added item"

      delete
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      delete ddbTableId event = do
        withId event $ \tid -> do
          let keys = SHM.fromList [
                  ("Id", attributeValue & avS .~ Just tid)
                ]
          deleteDdbRecord ddbTableId keys
          output "successfully deleted item"



