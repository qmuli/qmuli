{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as SHM
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.AWS.DynamoDB (AttributeValue, attributeValue, avS)

import           Qi.Util.DDB


idKeys
  :: (Hashable k, IsString k, Eq k)
  => Text
  -> SHM.HashMap k AttributeValue
idKeys cid = SHM.fromList [ ("Id", stringAttr cid) ]

