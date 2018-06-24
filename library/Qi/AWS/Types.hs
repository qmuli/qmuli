{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.AWS.Types where

import           Data.Aeson
import qualified Data.Text      as T
import           Protolude
import           Qi.AWS.Cognito


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


newtype Arn = Arn Text
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)




