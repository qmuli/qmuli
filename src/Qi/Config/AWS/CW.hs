{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.CW where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types     (Options (..), SumEncoding (..),
                                       fieldLabelModifier, typeMismatch)
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.AWS.DynamoDB (AttributeValue)

import           Qi.Config.Identifier


data CwEventsRule = CwEventsRule {
    _cerName    :: Text
  , _cerProfile :: CwEventsRuleProfile
  , _cerLbdId   :: LambdaId
  }

data CwConfig = CwConfig {
    _ccRules :: HashMap CwEventsRuleId CwEventsRule
  }

instance Default CwConfig where
  def = CwConfig {
    _ccRules = SHM.empty
  }



data CwEventsRuleProfile =
    ScheduledEventProfile {
    _csepSchedule :: Text
  }
  | PatternedEventProfile {
    _cpepPattern :: Text
  }

data CwEvent = CwEvent -- {}

instance FromJSON CwEvent where
  parseJSON v = pure CwEvent

makeLenses ''CwEventsRuleProfile
makeLenses ''CwEventsRule
makeLenses ''CwConfig


