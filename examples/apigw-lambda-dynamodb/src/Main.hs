{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM, void)
import           Data.Aeson
import           Data.Default                    (def)
import qualified Data.HashMap.Strict             as SHM
import           Data.Text                       (pack)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan

import           Qi                              (withConfig)
import           Qi.Config.AWS.ApiGw             (ApiMethodEvent (..),
                                                  ApiVerb (Delete, Get, Post),
                                                  RequestBody (..), aeBody,
                                                  aeParams, rpPath)
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..),
                                                  DdbProvCap (..))
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram)
import           Qi.Program.Config.Interface
import           Qi.Program.Lambda.Interface     (LambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, scanDdbRecords)
import           Qi.Util
import           Qi.Util.ApiGw
import           Qi.Util.DDB

import           Types

-- Used the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://6kkaf6fj99.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things"
curl -v -X GET "$API/things/cup"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"chair\", \"shape\": \"square\", \"size\": 10}" "$API/things"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/mycup"
curl -v -X GET "$API/things"
-}


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      thingsTable <- ddbTable "things" (DdbAttrDef "Id" S) Nothing (DdbProvCap 1 1)

      -- create a REST API
      api "world" >>= \world ->
        -- create a "things" resource
        apiResource "things" world >>= \things -> do
          -- create a GET method that is attached to the "scan" lambda
          apiMethodLambda "scanThings" Get
            things def (scan thingsTable) def

          apiMethodLambda "putThing" Post
            things def (put thingsTable) def

          -- create a "thingId" slug resource under "things"
          apiResource "{thingId}" things >>= \thing -> do

            apiMethodLambda "getThing" Get
              thing def (get thingsTable) def

            apiMethodLambda "deleteThing" Delete
              thing def (delete thingsTable) def

      return ()

    scan
      :: DdbTableId
      -> ApiMethodEvent
      -> LambdaProgram ()
    scan ddbTableId event = do
      r <- scanDdbRecords ddbTableId
      withSuccess (r^.srsResponseStatus) $
        result
          (internalError . ("Parsing error: " ++))
          (success . (toJSON :: [Thing] -> Value))
          $ forM (r^.srsItems) parseAttrs


    get
      :: DdbTableId
      -> ApiMethodEvent
      -> LambdaProgram ()
    get ddbTableId event =
      withPathParam "thingId" event $ \tid -> do
        r <- getDdbRecord ddbTableId $ byNameKey tid
        withSuccess (r^.girsResponseStatus) $
          result
            (internalError . ("Parsing error: " ++))
            (success . (toJSON :: Thing -> Value))
            $ parseAttrs $ r^.girsItem


    put
      :: DdbTableId
      -> ApiMethodEvent
      -> LambdaProgram ()
    put ddbTableId event =
      withDeserializedBody event $ \(thing :: Thing) -> do
        r <- putDdbRecord ddbTableId $ toAttrs thing
        withSuccess (r^.pirsResponseStatus) $
          successString "successfully put thing"


    delete
      :: DdbTableId
      -> ApiMethodEvent
      -> LambdaProgram ()
    delete ddbTableId event = do
      withPathParam "thingId" event $ \tid -> do
        r <- deleteDdbRecord ddbTableId $ byNameKey tid
        withSuccess (r^.dirsResponseStatus) $
          successString "successfully deleted thing"




