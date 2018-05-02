{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.CF where

import           Control.Lens
import           Control.Monad.Fail   (fail)
import           Data.Aeson
import           Data.Aeson.Types     (Options (..), SumEncoding (..),
                                       fieldLabelModifier, typeMismatch)
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           Protolude
import           Qi.Config.Identifier


data CfRequestType = CfCreate | CfUpdate | CfDelete

instance FromJSON CfRequestType where
  parseJSON = withText "CfRequestType" $ \case
    "Create" -> pure CfCreate
    "Update" -> pure CfUpdate
    "Delete" -> pure CfDelete
    unknown  -> fail $ "unknown request type: " <> toS unknown

data CfEvent = CfEventCreate {
    _cfeResponseURL        :: Text
  , _cfeStackId            :: Text
  , _cfeRequestId          :: Text
  , _cfeResourceType       :: Text
  , _cfeLogicalResourceId  :: Text
  , _cfeResourceProperties :: Object
  }
  | CfEventUpdate {
    _cfeResponseURL           :: Text
  , _cfeStackId               :: Text
  , _cfeRequestId             :: Text
  , _cfeResourceType          :: Text
  , _cfeLogicalResourceId     :: Text
  , _cfePhysicalResourceId    :: Text
  , _cfeResourceProperties    :: Object
  , _cfeOldResourceProperties :: Object
  }
  | CfEventDelete {
    _cfeResponseURL        :: Text
  , _cfeStackId            :: Text
  , _cfeRequestId          :: Text
  , _cfeResourceType       :: Text
  , _cfeLogicalResourceId  :: Text
  , _cfePhysicalResourceId :: Text
  , _cfeResourceProperties :: Object
  }
  deriving Generic


instance FromJSON CfEvent where
  parseJSON = genericParseJSON defaultOptions {
                  fieldLabelModifier = drop 4
                , sumEncoding = TaggedObject "RequestType" ""
                , constructorTagModifier = drop 7
                }


data Custom = Custom {
    _cLbdId :: LambdaId
  }

instance Hashable Custom where
  hashWithSalt s Custom{_cLbdId = LambdaId lid} = s `hashWithSalt` (show lid ++ "custom")


data CfConfig = CfConfig {
    _cfcCustoms :: HashMap CustomId Custom
  }

instance Default CfConfig where
  def = CfConfig {
    _cfcCustoms = SHM.empty
  }

makeLenses ''CfEvent
makeLenses ''CfConfig
makeLenses ''Custom
