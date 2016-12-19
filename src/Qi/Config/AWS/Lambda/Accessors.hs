{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Lambda.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdName, lcLambdas)
import           Qi.Config.Identifier


getLambdaLogicalNameFromId
  :: LambdaId
  -> Config
  -> Text
getLambdaLogicalNameFromId lid config =
  getLambdaLogicalName $ getLambdaById lid config

getLambdaLogicalName
  :: Lambda
  -> Text
getLambdaLogicalName lbd =
  T.concat [lbd ^. lbdName, "Lambda"]

getLambdaPermissionLogicalName
  :: Lambda
  -> Text
getLambdaPermissionLogicalName lbd =  T.concat [lbd ^. lbdName, "LambdaPermission"]


getFullLambdaName
  :: Lambda
  -> Config
  -> Text
getFullLambdaName lbd config =
  (lbd ^. lbdName) `underscoreNamePrefixWith` config


getLambdaById
  :: LambdaId
  -> Config
  -> Lambda
getLambdaById lid config =
  case SHM.lookup lid lbdMap of
    Just lbd -> lbd
    Nothing  -> error $ "Could not reference lambda with id: " ++ show lid
  where
    lbdMap = config^.lbdConfig.lcLambdas



getAllLambdas
  :: Config
  -> [Lambda]
getAllLambdas config =
  SHM.elems $ config ^. lbdConfig . lcLambdas




