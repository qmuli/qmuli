{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM, void, (<=<))
import           Data.Aeson
import qualified Data.HashMap.Strict             as SHM
import           Data.Text                       (pack)
import           Network.AWS.DynamoDB            (AttributeValue,
                                                  attributeValue, avS)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan

import           Qi                              (withConfig)
import           Qi.Config.AWS.Api               (ApiEvent (..),
                                                  ApiVerb (Delete, Get, Post),
                                                  RequestBody (..), aeBody,
                                                  aeParams, rpPath)
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..),
                                                  DdbProvCap (..))
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram)
import qualified Qi.Program.Config.Interface     as CI
import           Qi.Program.Lambda.Interface     (LambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, scanDdbRecords)
import           Qi.Util.Api

import           System.Environment
import           Types

-- Used the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://gliqqtz1pi.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"cup\", \"shape\": \"round\", \"size\": 3}" "$API/things/cup"
curl -v -X GET "$API/things/cup"
curl -v -X POST -H "Content-Type: application/json" -d "{\"name\": \"chair\", \"shape\": \"square\", \"size\": 10}" "$API/things/chair"
curl -v -X GET "$API/things"
curl -v -X DELETE "$API/things/mycup"
curl -v -X GET "$API/things"
-}


main :: IO ()
main = do
  args <- getArgs
  case args of
    (name:rest) -> withArgs rest (pack name `withConfig` config)
    []          -> putStrLn "Please provide a unique application name to deploy your Lambda"

    where
      config :: ConfigProgram ()
      config = do
        thingsTable <- CI.ddbTable "things" (DdbAttrDef "Id" S) Nothing (DdbProvCap 1 1)

        -- create a REST API
        CI.api "world" >>= \api ->
          -- create a "things" resource
          CI.apiResource "things" api >>= \things -> do
            -- create a GET method that is attached to the "scan" lambda
            CI.apiMethodLambda "scanThings" Get
              things $ scan thingsTable

            -- create a "thingId" slug resource under "things"
            CI.apiResource "{thingId}" things >>= \thing -> do

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
        res <- scanDdbRecords ddbTableId
        case res^.srsResponseStatus of
          200 -> do
            let thingsResult :: Result [Thing] = forM (res^.srsItems) $ \ddbRecord ->
                  case SHM.lookup "Data" ddbRecord of
                    Just thingAttrs -> do
                      castFromDdbAttrs unDdbThing thingAttrs

                    Nothing ->
                      Error $ "Error: could not extract thing data, DDB record was: " ++ show ddbRecord

            case thingsResult of
              Success things ->
                success $ toJSON things
              Error err ->
                internalError $ "Parsing error: " ++ show err

          unexpected ->
            internalError $ "Error: unexpected response status: " ++ show unexpected




      get
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      get ddbTableId event = do
        withId event $ \tid -> do
          let keys = SHM.fromList [
                  ("Id", attributeValue & avS .~ Just tid)
                ]
          res <- getDdbRecord ddbTableId keys
          case res^.girsResponseStatus of
            200 -> do
              let item = res^.girsItem
              if item == SHM.fromList []
                then -- no found item by that key
                  notFoundError "no such thing found"
                else
                  case SHM.lookup "Data" item of
                    Just thingAttrs -> do
                      case castFromDdbAttrs unDdbThing thingAttrs of
                        Success (thing :: Thing)  ->
                          success $ toJSON thing
                        Error err                 ->
                          internalError $ "Error: fromJson: " ++ err ++ ". Attrs were: " ++ show thingAttrs

                    Nothing ->
                      internalError $ "Error: could not extract thing data, DDB record was: " ++ show item

            unexpected ->
              internalError $ "Error: unexpected response status: " ++ show unexpected

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

            res <- putDdbRecord ddbTableId item
            case res^.pirsResponseStatus of
              200 -> do
                successString "successfully put item"

              unexpected ->
                internalError $ "Error: unexpected response status: " ++ show unexpected

      delete
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      delete ddbTableId event = do
        withId event $ \tid -> do
          let keys = SHM.fromList [
                  ("Id", attributeValue & avS .~ Just tid)
                ]
          res <- deleteDdbRecord ddbTableId keys
          case res^.dirsResponseStatus of
            200 -> do
              successString "successfully deleted item"

            unexpected ->
              internalError $ "Error: unexpected response status: " ++ show unexpected



withThingAttributes
  :: ApiEvent
  -> (AttributeValue -> LambdaProgram ())
  -> LambdaProgram ()
withThingAttributes event f = case event^.aeBody of
  JsonBody jb -> case castToDdbAttrs DdbThing jb of
    Success thing -> f thing
    Error err     -> internalError $ "Error: fromJson: " ++ err ++ ". Json was: " ++ show jb
  unexpected  ->
    argumentsError $ "Unexpected request body: " ++ show unexpected


withId event f = case SHM.lookup "thingId" $ event^.aeParams.rpPath of
  Just (String x) -> f x
  Just unexpected ->
    argumentsError $ "unexpected path parameter: " ++ show unexpected
  Nothing ->
    argumentsError "expected path parameter 'thingId' was not found"


