{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Lens                hiding (view, (.=))
import           Control.Monad               ((<=<))
import           Data.Aeson
import           Data.Aeson.Types            (typeMismatch)
import qualified Data.ByteString.Char8       as BS
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Network.AWS.DynamoDB        (AttributeValue)

import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Delete, Get, Post),
                                              RequestBody (..), aeBody,
                                              aeParams, rpPath)
import           Qi.Config.AWS.DDB           (DdbAttrDef (..), DdbAttrType (..),
                                              DdbProvCap (..))
import           Qi.Program.Lambda.Interface (LambdaProgram, output)

import           Types


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
