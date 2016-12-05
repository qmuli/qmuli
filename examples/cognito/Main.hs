{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM, void)
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
import           Qi.Program.Config.Interface     (ConfigProgram, customResource)
import           Qi.Program.Lambda.Interface     (LambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, scanDdbRecords)
import           Qi.Util.Api

import           System.Environment              (getArgs, withArgs)
import           Types


data Response = Response {
    rStatus             :: Text
  , rReason             :: Text
  , rPhysicalResourceId :: Text
  , rStackId            :: Text
  , rRequestId          :: Text
  , rLogicalResourceId  :: Text
  , rData               :: Text
}

main :: IO ()
main = do
  args <- getArgs
  case args of
    (appName:rest) -> withArgs rest $ (pack appName) `withConfig` config
    _              -> putStrLn "Please provide a unique application name for your qmulus"

    where
      config :: ConfigProgram ()
      config = do
        custom <- customResource
        void . customResourceLambda "cognitoIdentityPoolProvider" $ cognitoIdentityPoolProvider custom

      cognitoIdentityPoolProvider
        :: CustomResourceId
        -> CFEvent
        -> LambdaProgram ()
      cognitoIdentityPoolProvider ddbTableId event = do
        res <- scanDdbRecords ddbTableId
        case res^.srsResponseStatus of
          200 -> do

            success resp

          unexpected ->
            internalError $ "Error: unexpected response status: " ++ show unexpected
