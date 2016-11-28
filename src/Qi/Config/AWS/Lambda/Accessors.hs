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


getLambdaCFResourceNameFromId
  :: LambdaId
  -> Config
  -> Text
getLambdaCFResourceNameFromId lid config =
  getLambdaCFResourceName $ getLambdaById lid config

getLambdaCFResourceName
  :: Lambda
  -> Text
getLambdaCFResourceName lbd =
  T.concat [lbd ^. lbdName, "Lambda"]

getLambdaPermissionCFResourceName
  :: Lambda
  -> Text
getLambdaPermissionCFResourceName lbd =  T.concat [lbd ^. lbdName, "LambdaPermission"]


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




