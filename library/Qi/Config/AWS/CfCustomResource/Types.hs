{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


module Qi.Config.AWS.CfCustomResource.Types where

import           Control.Lens               hiding (view, (.=))
import           Control.Monad.Fail         (fail)
import           Data.Aeson                 hiding (Result)
import           Data.Aeson.Types           (fieldLabelModifier, typeMismatch)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as SHM
import qualified Data.Text                  as T
import           GHC.Generics

import           Network.HTTP.Client        (Request (..), RequestBody (..),
                                             parseRequest_)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Protolude
import           Qi.AWS.CF
import           Qi.AWS.Types
import           Qi.Config.AWS.CF


-- TODO: fix this sum data type
data CfCustomResourceEvent =
    CfCustomResourceCreate {
    _cfeServiceToken       :: Arn
  , _cfeResponseURL        :: Text
  , _cfeStackId            :: Arn
  , _cfeRequestId          :: Text
  , _cfeLogicalResourceId  :: LogicalResourceId
  , _cfeResourceType       :: Text
  , _cfeResourceProperties :: Object
  }
  | CfCustomResourceUpdate {
    _cfeResponseURL           :: Text
  , _cfeStackId               :: Arn
  , _cfeRequestId             :: Text
  , _cfeResourceType          :: Text
  , _cfeLogicalResourceId     :: LogicalResourceId
  , _cfePhysicalResourceId    :: CustomResourceId
  , _cfeResourceProperties    :: Object
  , _cfeOldResourceProperties :: Object
  }
  | CfCustomResourceDelete {
    _cfeResponseURL        :: Text
  , _cfeStackId            :: Arn
  , _cfeRequestId          :: Text
  , _cfeResourceType       :: Text
  , _cfeLogicalResourceId  :: LogicalResourceId
  , _cfePhysicalResourceId :: CustomResourceId
  , _cfeResourceProperties :: Object
  }
  deriving (Eq, Show, Generic)



instance FromJSON CfCustomResourceEvent where
  parseJSON = genericParseJSON defaultOptions {
                  fieldLabelModifier = drop 4
                , sumEncoding = TaggedObject "RequestType" ""
                , constructorTagModifier = drop 16
                }


data CfCustomResourceRequestType =
    CfCustomResourceRequestCreate
  | CfCustomResourceRequestUpdate
  | CfCustomResourceRequestDelete

instance FromJSON CfCustomResourceRequestType where
  parseJSON = withText "CfCustomResourceRequestType" $ \case
    "Create" -> pure CfCustomResourceRequestCreate
    "Update" -> pure CfCustomResourceRequestUpdate
    "Delete" -> pure CfCustomResourceRequestDelete
    unknown  -> fail $ "unknown request type: " <> toS unknown


data CustomResourceStatus = CustomResourceSuccess | CustomResourceFailure
instance ToJSON CustomResourceStatus where
  toJSON CustomResourceSuccess = String "SUCCESS"
  toJSON CustomResourceFailure = String "FAILED"


data Response = Response {
    rStatus             :: CustomResourceStatus
  , rReason             :: Text
  , rPhysicalResourceId :: Maybe CustomResourceId
  , rStackId            :: Arn
  , rRequestId          :: Text
  , rLogicalResourceId  :: LogicalResourceId
  , rData               :: Object
  } deriving Generic

instance ToJSON Response where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = drop 1 }


data Result = Result {
    rId    :: Maybe CustomResourceId
  , rAttrs :: Object
  }


makeLenses ''CfCustomResourceEvent

