{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.AWS.Types where

import           Data.Aeson
import qualified Data.Text      as T
import           Network.AWS    (Logger)
import           Protolude
import           Qi.AWS.Cognito


newtype Arn = Arn Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)


data AwsMode = RealDeal | LocalStack
  deriving Eq

type MkAwsLogger = IO Logger

