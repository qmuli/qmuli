{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util.Api where

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






