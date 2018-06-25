{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Qi.AWS.CF where

import           Control.Monad.Fail (fail)
import           Data.Aeson
import qualified Data.Text          as T
import           Protolude
import           Qi.AWS.Cognito
import           Qi.AWS.Lex
import           Qi.AWS.Types


newtype LogicalResourceId = LogicalResourceId Text
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)


newtype CustomResourceId = CustomResourceId (NonEmpty PhysicalResourceId)
  deriving (Eq, Show, Read, Generic)
  deriving newtype (Semigroup)


-- The ugliness below is because AWS requires the PhysicalId field to be
-- necessarily a String json type
instance ToJSON CustomResourceId where
  toJSON = String . show

instance FromJSON CustomResourceId where
  parseJSON = withText "CustomResourceId" $ \(toS -> s) ->
    case readMaybe s of
      Just cr ->
        pure cr
      Nothing ->
        fail $ "could not parse CustomResourceId from: '" <> s <> "'"


data PhysicalResourceId =
    ArnResourceId Arn
  | AuthRoleIdResourceId AuthRoleId
  | UserPoolIdResourceId UserPoolId
  | UserPoolClientIdResourceId UserPoolClientId
  | IdPoolIdResourceId IdPoolId
  | BotResourceId LatestBotDeployment
  | UnknownResourceIdType Text
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


