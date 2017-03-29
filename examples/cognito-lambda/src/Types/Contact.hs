{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Contact where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.Aeson.Types     (fieldLabelModifier, typeMismatch)
import           Data.Char            (toLower)
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.AWS.DynamoDB (AttributeValue, attributeValue, avS)

import           Qi.Util.DDB

import           Types


data Contact = Contact {
    cId        :: Text
  , cFirstName :: Text
  , cLastName  :: Text
} deriving (Generic, Show)

instance Hashable Contact where
  s `hashWithSalt` Contact{cFirstName, cLastName} = s `hashWithSalt` T.concat [cFirstName, cLastName]


instance ToJSON Contact where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = drop 1 . fmap toLower }

instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 . fmap toLower }


instance FromAttrs Contact where
  parseAttrs hm = Contact
    <$> parseStringAttr "Id" hm
    <*> parseStringAttr "FirstName" hm
    <*> parseStringAttr "LastName" hm

instance ToAttrs Contact where
  toAttrs Contact{cId, cFirstName, cLastName} =
    SHM.fromList [
                    ("Id",        stringAttr cId)
                  , ("FirstName", stringAttr cFirstName)
                  , ("LastName",  stringAttr cLastName)
                  ]




