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
import           GHC.Show             (Show (..))
import           Network.AWS.DynamoDB (AttributeValue)
import           Protolude

import           Qi.Config.Identifier


type DdbAttrs = HashMap Text AttributeValue


data DdbStreamEvent = DdbStreamEvent {
    _dseData :: Value
  }
  deriving (Eq, Show)

instance FromJSON DdbStreamEvent where
  parseJSON = pure . DdbStreamEvent


data DdbAttrType = S | N | B
  deriving (Eq, Show)


data DdbAttrDef = DdbAttrDef {
    _daName :: Text
  , _daType :: DdbAttrType
  }
  deriving (Eq, Show)


data DdbTable = DdbTable {
    _dtName          :: Text
  , _dtHashAttrDef   :: DdbAttrDef
  , _dtProfile       :: DdbTableProfile
  , _dtStreamHandler :: Maybe LambdaId
  }
  deriving (Eq, Show)


data DdbConfig = DdbConfig {
    _dcTables :: HashMap DdbTableId DdbTable
  }
  deriving (Eq, Show)

instance Default DdbConfig where
  def = DdbConfig {
    _dcTables = SHM.empty
  }


data DdbProvCap = DdbProvCap {
    _dpcRead  :: Integer
  , _dpcWrite :: Integer
  }
  deriving (Eq, Show)

instance Default DdbProvCap where
  def = DdbProvCap {
    _dpcRead  = 2
  , _dpcWrite = 2
  }

data DdbTableProfile = DdbTableProfile {
    _dtpRangeKey :: Maybe DdbAttrDef
  , _dtpProvCap  :: DdbProvCap
  }
  deriving (Eq, Show)

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

