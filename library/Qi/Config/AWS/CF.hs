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
import           Qi.AWS.Types
import           Qi.Config.Identifier


data CfRequestType = CfCreate | CfUpdate | CfDelete

instance FromJSON CfRequestType where
  parseJSON = withText "CfRequestType" $ \case
    "Create" -> pure CfCreate
    "Update" -> pure CfUpdate
    "Delete" -> pure CfDelete
    unknown  -> fail $ "unknown request type: " <> toS unknown


{-
{
  "RequestType": "Create",
  "ServiceToken": "arn:aws:lambda:us-east-1:910653408535:function:cognitotestxx_cognitoPoolProvider",
  "ResponseURL": "https://cloudformation-custom-resource-response-useast1.s3.amazonaws.com/arn%3Aaws%3Acloudformation%3Aus-east-1%3A910653408535%3Astack/cognitotestxx/3b615fb0-7701-11e8-85ef-50fa5f2588d2%7CcognitoPoolProviderLambdaCustom%7C9616e4f8-9153-4bc6-bc02-53252002a8a3?AWSAccessKeyId=AKIAJGHTPRZJWHX6QVGA&Expires=1529778000&Signature=i3hplxObOjPQvet9UGeiCQZJ60A%3D\",
  "StackId": "arn:aws:cloudformation:us-east-1:910653408535:stack/cognitotestxx/3b615fb0-7701-11e8-85ef-50fa5f2588d2",
  "RequestId": "9616e4f8-9153-4bc6-bc02-53252002a8a3",
  "LogicalResourceId": "cognitoPoolProviderLambdaCustom",
  "ResourceType": "AWS::CloudFormation::CustomResource",
  "ResourceProperties": {
    "ServiceToken": "arn:aws:lambda:us-east-1:910653408535:function:cognitotestxx_cognitoPoolProvider"
  }
}
 -}


-- TODO: fix this sum data type
data CfEvent = CfEventCreate {
    _cfeServiceToken       :: Arn
  , _cfeResponseURL        :: Text
  , _cfeStackId            :: Arn
  , _cfeRequestId          :: Text
  , _cfeLogicalResourceId  :: LogicalResourceId
  , _cfeResourceType       :: Text
  , _cfeResourceProperties :: Object
  }
  | CfEventUpdate {
    _cfeResponseURL           :: Text
  , _cfeStackId               :: Arn
  , _cfeRequestId             :: Text
  , _cfeResourceType          :: Text
  , _cfeLogicalResourceId     :: LogicalResourceId
  , _cfePhysicalResourceId    :: CompositeResourceId
  , _cfeResourceProperties    :: Object
  , _cfeOldResourceProperties :: Object
  }
  | CfEventDelete {
    _cfeResponseURL        :: Text
  , _cfeStackId            :: Arn
  , _cfeRequestId          :: Text
  , _cfeResourceType       :: Text
  , _cfeLogicalResourceId  :: LogicalResourceId
  , _cfePhysicalResourceId :: CompositeResourceId
  , _cfeResourceProperties :: Object
  }
  deriving (Eq, Show, Generic)


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
