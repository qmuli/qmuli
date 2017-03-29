{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM)
import           Data.Aeson
import           Data.Default                    (def)
import qualified Data.Text                       as T
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan

import           Qi                              (withConfig)
import           Qi.Config.AWS.ApiGw             (ApiVerb (Delete, Get, Post))
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..))
import           Qi.Config.AWS.Lambda            (LambdaMemorySize (..),
                                                  lpMemorySize)
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram, api,
                                                  apiMethodLambda, apiResource,
                                                  ddbTable)
import           Qi.Program.Lambda.Interface     (ApiLambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, say,
                                                  scanDdbRecords)
import           Qi.Util                         (internalError, result,
                                                  success, withSuccess)
import           Qi.Util.ApiGw
import           Qi.Util.DDB

import           Types

-- Used the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://5p07mx81w8.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things"
curl -v -X GET "$API/things/cup"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"chair\", \"shape\": \"square\", \"size\": 10}" "$API/things"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/cup"
curl -v -X GET "$API/things"
-}


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      thingsTable <- ddbTable "things" (DdbAttrDef "name" S) def

      let apiLambdaConfig = def & lpMemorySize .~ M1536

      -- create a REST API
      api "world" >>= \world ->
        -- create a "things" resource
        apiResource "things" world >>= \things -> do
          -- create a GET method that is attached to the "scan" lambda
          apiMethodLambda "scanThings" Get
            things def (scan thingsTable) apiLambdaConfig

          apiMethodLambda "putThing" Post
            things def (put thingsTable) apiLambdaConfig

          -- create a "thingId" slug resource under "things"
          apiResource "{thingId}" things >>= \thing -> do

            apiMethodLambda "getThing" Get
              thing def (get thingsTable) apiLambdaConfig

            apiMethodLambda "deleteThing" Delete
              thing def (delete thingsTable) apiLambdaConfig

      return ()

    scan
      :: DdbTableId
      -> ApiLambdaProgram
    scan ddbTableId _ = do
      say "scanning records..."
      r <- scanDdbRecords ddbTableId
      say $ T.concat ["got scan response: ", T.pack $ show r]
      withSuccess (r^.srsResponseStatus) $
        result
          (internalError . ("Parsing error: " ++))
          (success . (toJSON :: [Thing] -> Value))
          $ forM (r^.srsItems) parseAttrs


    get
      :: DdbTableId
      -> ApiLambdaProgram
    get ddbTableId event =
      withPathParam "thingId" event $ \tid -> do
        say $ T.concat ["getting record with thingId: '", tid, "'..."]
        r <- getDdbRecord ddbTableId $ byNameKey tid
        withSuccess (r^.girsResponseStatus) $
          result
            (internalError . ("Parsing error: " ++))
            (success . (toJSON :: Thing -> Value))
            $ parseAttrs $ r^.girsItem


    put
      :: DdbTableId
      -> ApiLambdaProgram
    put ddbTableId event =
      withDeserializedBody event $ \(thing :: Thing) -> do
        say $ T.concat ["putting record: ", T.pack $ show thing, "..."]
        r <- putDdbRecord ddbTableId $ toAttrs thing
        withSuccess (r^.pirsResponseStatus) $
          success "successfully put thing"


    delete
      :: DdbTableId
      -> ApiLambdaProgram
    delete ddbTableId event = do
      withPathParam "thingId" event $ \tid -> do
        say $ T.concat ["deletting record with thingId: '", tid, "'..."]
        r <- deleteDdbRecord ddbTableId $ byNameKey tid
        withSuccess (r^.dirsResponseStatus) $
          success "successfully deleted thing"




