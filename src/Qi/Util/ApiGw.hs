{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util.ApiGw where

import           Control.Lens                hiding (view, (.=))
import           Data.Aeson
import           Data.Aeson.Types            (typeMismatch)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import qualified Data.HashMap.Strict         as SHM
import qualified Data.Text                   as T
import           Network.AWS.DynamoDB        (AttributeValue)
import           Protolude
import           Qi.Config.AWS.ApiGw         (ApiMethodEvent (..),
                                              ApiVerb (Delete, Get, Post),
                                              RequestBody (..), aeBody,
                                              aeParams, rpPath)
import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Program.Lambda.Interface (CompleteLambdaProgram,
                                              LambdaProgram)
import           Qi.Util


withPathParam
  :: Text
  -> ApiMethodEvent
  -> (Text -> CompleteLambdaProgram)
  -> CompleteLambdaProgram
withPathParam name event f = case SHM.lookup name $ event^.aeParams.rpPath of
  Just (String x) -> f x
  Just unexpected ->
    argumentsError $ "unexpected path parameter: " ++ show unexpected
  Nothing ->
    argumentsError "expected path parameter 'thingId' was not found"

withDeserializedBody
  :: FromJSON a
  => ApiMethodEvent
  -> (a -> LambdaProgram LBS.ByteString)
  -> LambdaProgram LBS.ByteString
withDeserializedBody event f = case event^.aeBody of
  JsonBody jb ->
    result
      (internalError . ("Error: fromJson: " <>))
      f
      $ fromJSON jb
  unexpected  ->
    argumentsError $ "Unexpected request body: " <> show unexpected
