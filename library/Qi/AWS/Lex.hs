{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Qi.AWS.Lex where

import           Data.Aeson
import qualified Data.Text    as T
import           Protolude
import           Qi.AWS.Types


newtype BotName = BotName Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype BotVersion = BotVersion Text
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

newtype BotChecksum = BotChecksum Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)


data LatestBotDeployment =
  LatestBotDeployment BotName BotChecksum
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)



data EnumVal = EnumVal {
    evName     :: Text
  , evSynonyms :: [Text]
  }
  deriving (Eq, Show)

instance ToJSON EnumVal where
  toJSON EnumVal{ evName, evSynonyms } = object [
      "value"     .= evName
    , "resource"  .= evSynonyms
    ]


newtype SlotTypeName = SlotTypeName Text
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

newtype SlotTypeVersion = SlotTypeVersion Text
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)


data SlotType = SlotType {
    stName     :: SlotTypeName
  , stVersion  :: SlotTypeVersion
  , stEnumVals :: [EnumVal]
  }
  deriving (Eq, Show)

instance ToJSON SlotType where
  toJSON SlotType{ stName, stVersion, stEnumVals } = object [
      "metadata"      .= metadata
    , "resource"      .= resource
    ]
    where
      metadata = object [
          "schemaVersion" .= String "1.0"
        , "importType"    .= String "LEX"
        , "importFormat"  .= String "JSON"
        ]
      resource = object [
          "name"              .= toJSON stName
        , "version"           .= toJSON stVersion
        , "enumerationValues" .= toJSON stEnumVals
        , "valueSelectionStrategy" .= String "???"
        ]



newtype IntentName = IntentName Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)

data Intent = Intent {
    iName :: IntentName
  }
  deriving (Eq, Show)

instance ToJSON Intent where
  toJSON Intent{ iName } = object [
      "metadata"      .= metadata
    , "resource"      .= resource
    ]
    where
      metadata = object [
          "schemaVersion" .= String "1.0"
        , "importType"    .= String "LEX"
        , "importFormat"  .= String "JSON"
        ]
      resource = object [
          "name"      .= toJSON iName
        , "version"   .= String "1"
        ]




data BotSpec = BotSpec {
    bName      :: BotName
  , bIntents   :: [Intent]
  , bSlotTypes :: [SlotType]
  }
  deriving (Eq, Show)

instance ToJSON BotSpec where
  toJSON BotSpec{ bName, bIntents, bSlotTypes } = object [
      "metadata"      .= metadata
    , "resource"      .= resource
    ]
    where
      metadata = object [
          "schemaVersion" .= String "1.0"
        , "importType"    .= String "LEX"
        , "importFormat"  .= String "JSON"
        ]
      resource = object [
          "name"      .= toJSON bName
        , "version"   .= String "1"
        , "intents"   .= toJSON bIntents
        , "slotTypes" .= toJSON bSlotTypes
        ]

