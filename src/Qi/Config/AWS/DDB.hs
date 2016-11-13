{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.DDB where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.Hashable
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
  deriving Show

data DdbProvCap = DdbProvCap {
    _dpcRead  :: Integer
  , _dpcWrite :: Integer
  }
  deriving Show

data DdbTable = DdbTable {
    _dtName         :: Text
  , _dtHashAttrDef  :: DdbAttrDef
  , _dtRangeAttrDef :: Maybe DdbAttrDef
  , _dtProvCap      :: DdbProvCap
  }
  deriving Show

instance Hashable DdbTable where
  hashWithSalt s DdbTable{_dtName} = s `hashWithSalt` _dtName


data DdbConfig = DdbConfig {
    _dcTables :: HashMap DdbTableId DdbTable
  } deriving Show

instance Monoid DdbConfig where
  DdbConfig { _dcTables = t1 } `mappend` DdbConfig { _dcTables = t2 } =
    DdbConfig { _dcTables = t1 `mappend` t2 }
  mempty = def

instance Default DdbConfig where
  def = DdbConfig {
    _dcTables = SHM.empty
  }

makeLenses ''DdbAttrDef
makeLenses ''DdbProvCap
makeLenses ''DdbTable
makeLenses ''DdbConfig

