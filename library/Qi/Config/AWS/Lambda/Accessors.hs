{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Lambda.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda)
import           Qi.Config.Identifier


getPermissionLogicalName
  :: Config
  -> Lambda
  -> Text
getPermissionLogicalName config r =
  T.concat [getName config r, "LambdaPermission"]


