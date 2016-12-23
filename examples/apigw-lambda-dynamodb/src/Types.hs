{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Hashable        (Hashable, hashWithSalt)
import           Data.HashMap.Strict  (HashMap)
import           Data.String          (IsString)
import           Data.Text            (Text)
import           GHC.Generics
import           Network.AWS.DynamoDB (AttributeValue)

import           Qi.Util.DDB


data Thing = Thing {
    name  :: Text
  , shape :: Text
  , size  :: Int
} deriving (Generic, Show)

instance ToJSON Thing where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Thing where

instance Hashable Thing where
  s `hashWithSalt` Thing{name} = s `hashWithSalt` name

instance FromAttrs Thing where
  parseAttrs hm = Thing
    <$> parseStringAttr "name"  hm
    <*> parseStringAttr "shape" hm
    <*> parseNumberAttr "size"  hm

instance ToAttrs Thing where
  toAttrs Thing{name, shape, size} = [
                    ("name",  stringAttr name)
                  , ("shape", stringAttr shape)
                  , ("size",  numberAttr size)
                  ]

byNameKey
  :: (Hashable k, IsString k, Eq k)
  => Text
  -> HashMap k AttributeValue
byNameKey name = [
    ("name", stringAttr name)
  ]


