{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.AWS.CF where

import           Data.Aeson
import qualified Data.Text      as T
import           Protolude
import           Qi.AWS.Cognito
import           Qi.AWS.Types


newtype LogicalResourceId = LogicalResourceId Text
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)


newtype CompositeResourceId = CompositeResourceId (NonEmpty PhysicalResourceId)
  deriving (Eq, Show, Generic, Semigroup)
  deriving newtype (ToJSON, FromJSON)


data PhysicalResourceId =
    ArnResourceId Arn
  | AuthRoleIdResourceId AuthRoleId
  | UserPoolIdResourceId UserPoolId
  | UserPoolClientIdResourceId UserPoolClientId
  | IdPoolIdResourceId IdPoolId
  | UnknownResourceIdType Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


