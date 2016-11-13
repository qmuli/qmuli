{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                hiding (view)
import           Control.Monad               (void)
import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.HashMap.Strict         as SHM
import           Data.Maybe                  (fromJust)
import           Data.Text.Encoding          (encodeUtf8)
import           Network.AWS.DynamoDB        (attributeValue, avM, avS)

import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Get, Post),
                                              RequestBody (..), aeBody,
                                              aeParams, rpPath)
import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Config.Identifier        (DdbTableId, S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram)
import qualified Qi.Program.Config.Interface as CI
import           Qi.Program.Lambda.Interface (LambdaProgram, getDdbRecord,
                                              output, putDdbRecord)

-- Used the two curl commands below to test-drive the two endpoints (substitute your unique api stage url first):
--
-- curl -v -X POST -H "Content-Type: application/json" -d "{\"S\": \"hello there\"}" "https://2gezp5kxjb.execute-api.us-east-1.amazonaws.com/v1/things/xyz"
-- curl -v -X GET "https://2gezp5kxjb.execute-api.us-east-1.amazonaws.com/v1/things/xyz"
--

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
          "getProp"
          Get
          thing
          $ getPropLambda thingsTable

        void $ CI.apiMethodLambda
          "putProp"
          Post
          thing
          $ putPropLambda thingsTable


      getPropLambda
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      getPropLambda ddbTableId event@ApiEvent{} = do
        withId event $ \tid -> do
          let keys = SHM.fromList [
                  ("Id", attributeValue & avS .~ Just tid)
                ]
          thing <- getDdbRecord ddbTableId keys
          case SHM.lookup "Data" thing of
            Just d ->
              output $ LBS.toStrict $ encode d
            Nothing ->
              output . BS.pack $ "Error: could not extract thing data, thing was: " ++ show thing



      putPropLambda
        :: DdbTableId
        -> ApiEvent
        -> LambdaProgram ()
      putPropLambda ddbTableId event@ApiEvent{} = do
        withId event $ \tid -> do
          withData event $ \tdata -> do
            let item = SHM.fromList [
                    ("Id",    attributeValue & avS .~ Just tid)
                  , ("Data",  tdata)
                  ]

            putDdbRecord ddbTableId item
            output "successfully added item"


      withData event f = case event^.aeBody of
        JsonBody jb -> case fromJSON jb of
          Success x -> f x
          Error err -> output . BS.pack $ "Error: fromJson: " ++ err
        unexpected ->
          output . BS.pack $ "Unexpected request body: " ++ show unexpected

      withId event f = case SHM.lookup "thingId" $ event^.aeParams.rpPath of
        Just (String x) -> f x
        Just unexpected ->
          output $ BS.pack $ "unexpected path parameter: " ++ show unexpected
        Nothing ->
          output "expected path parameter 'thingId' was not found"



