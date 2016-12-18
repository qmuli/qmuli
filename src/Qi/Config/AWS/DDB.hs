{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.DDB where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.AWS.DynamoDB (AttributeValue)

import           Qi.Config.Identifier


type DdbAttrs = HashMap Text AttributeValue

data DdbAttrType = S | N | B
  deriving Show

data DdbAttrDef = DdbAttrDef {
    _daName :: Text
  , _daType :: DdbAttrType
  }

data DdbProvCap = DdbProvCap {
    _dpcRead  :: Integer
  , _dpcWrite :: Integer
  }

data DdbTable = DdbTable {
    _dtName         :: Text
  , _dtHashAttrDef  :: DdbAttrDef
  , _dtRangeAttrDef :: Maybe DdbAttrDef
  , _dtProvCap      :: DdbProvCap
  }


data DdbConfig = DdbConfig {
    _dcTables :: HashMap DdbTableId DdbTable
  }


instance Default DdbConfig where
  def = DdbConfig {
    _dcTables = SHM.empty
  }

makeLenses ''DdbAttrDef
makeLenses ''DdbProvCap
makeLenses ''DdbTable
makeLenses ''DdbConfig

