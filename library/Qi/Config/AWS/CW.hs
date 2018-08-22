{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.CW where

import           Control.Lens
import           Control.Monad.Freer
import           Data.Aeson
import           Data.Aeson.Types           (Options (..), SumEncoding (..),
                                             fieldLabelModifier, typeMismatch)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default               (Default, def)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as SHM
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.AWS.DynamoDB       (AttributeValue)
import           Protolude
import           Qi.Config.Identifier


type CwLambdaProgram effs = CwEvent -> Eff effs LBS.ByteString

data CwEventsRule = CwEventsRule {
    _cerName    :: Text
  , _cerProfile :: CwEventsRuleProfile
  , _cerLbdId   :: LambdaId
  }
  deriving (Eq, Show)

data CwConfig = CwConfig {
    _ccRules :: HashMap CwEventsRuleId CwEventsRule
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data CwEvent = CwEvent -- {}
  deriving (Eq, Show)

instance FromJSON CwEvent where
  parseJSON v = pure CwEvent

makeLenses ''CwEventsRuleProfile
makeLenses ''CwEventsRule
makeLenses ''CwConfig


