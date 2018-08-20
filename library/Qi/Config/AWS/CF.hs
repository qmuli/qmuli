{-# LANGUAGE DeriveGeneric     #-}
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


data CfCustomResource = CfCustomResource {
    _cLbdId :: LambdaId
  }
  deriving (Eq, Show)

instance Hashable CfCustomResource where
  hashWithSalt s CfCustomResource{ _cLbdId = LambdaId lbdId } =
    s `hashWithSalt` (show lbdId <> "custom" :: Text)


data CfConfig = CfConfig {
    _cfcCustomResources :: HashMap CfCustomResourceId CfCustomResource
  }
  deriving (Eq, Show)

instance Default CfConfig where
  def = CfConfig {
    _cfcCustomResources = SHM.empty
  }

makeLenses ''CfConfig
makeLenses ''CfCustomResource
