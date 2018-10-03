{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.AWS.Cognito where

import           Data.Aeson
import           Protolude


newtype IdPoolId = IdPoolId Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype AuthRoleId = AuthRoleId Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype UserPoolId = UserPoolId Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype UserPoolClientId = UserPoolClientId Text
  deriving (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)


