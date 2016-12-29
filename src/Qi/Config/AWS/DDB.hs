{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.DDB where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.AWS.DynamoDB (AttributeValue)

import           Qi.Config.Identifier


type DdbAttrs = HashMap Text AttributeValue


data DdbStreamEvent = DdbStreamEvent {
    _dseData :: Value
  } deriving Show

instance FromJSON DdbStreamEvent where
  parseJSON = pure . DdbStreamEvent


data DdbAttrType = S | N | B
  deriving Show

data DdbAttrDef = DdbAttrDef {
    _daName :: Text
  , _daType :: DdbAttrType
  }


data DdbTable = DdbTable {
    _dtName          :: Text
  , _dtHashAttrDef   :: DdbAttrDef
  , _dtProfile       :: DdbTableProfile
  , _dtStreamHandler :: Maybe LambdaId
  }


data DdbConfig = DdbConfig {
    _dcTables :: HashMap DdbTableId DdbTable
  }

instance Default DdbConfig where
  def = DdbConfig {
    _dcTables = SHM.empty
  }


data DdbProvCap = DdbProvCap {
    _dpcRead  :: Integer
  , _dpcWrite :: Integer
  }

instance Default DdbProvCap where
  def = DdbProvCap {
    _dpcRead  = 2
  , _dpcWrite = 2
  }

data DdbTableProfile = DdbTableProfile {
    _dtpRangeKey :: Maybe DdbAttrDef
  , _dtpProvCap  :: DdbProvCap
  }

instance Default DdbTableProfile where
  def = DdbTableProfile {
    _dtpRangeKey  = Nothing
  , _dtpProvCap   = def
  }


makeLenses ''DdbAttrDef
makeLenses ''DdbTable
makeLenses ''DdbProvCap
makeLenses ''DdbTableProfile
makeLenses ''DdbConfig

