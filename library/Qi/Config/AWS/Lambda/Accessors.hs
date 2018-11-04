{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Lambda.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Protolude
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdNameToId)
import           Qi.Config.Identifier


getPermissionLogicalName
  :: Config
  -> Lambda
  -> Text
getPermissionLogicalName config r =
  getName config r <> "LambdaPermission"

getIdByName
  :: Config
  -> Text
  -> LambdaId
getIdByName config name =
  case SHM.lookup name nameToIdMap of
    Just bid -> bid
    Nothing  -> panic $ "Could not find LambdaId with name: " <> show name <>
      " following lambdas were found in the registry: " <> show (SHM.keys nameToIdMap)
  where
    nameToIdMap = config ^. lbdConfig . lbdNameToId
