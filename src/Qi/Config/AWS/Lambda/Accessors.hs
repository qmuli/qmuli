{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Lambda.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdName, lcLambdas)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier


getLambdaById
  :: LambdaId
  -> Config
  -> Lambda
getLambdaById lid =
    fromJust . SHM.lookup lid . (^.lbdConfig.lcLambdas)

getLambdaResourceNameFromId
  :: LambdaId
  -> Config
  -> Text
getLambdaResourceNameFromId lid config =
  getLambdaResourceName $ getLambdaById lid config

getLambdaResourceName
  :: Lambda
  -> Text
getLambdaResourceName lbd =
  T.concat [lbd ^. lbdName, "Lambda"]


getAllLambdas
  :: Config
  -> [Lambda]
getAllLambdas config =
  SHM.elems $ config ^. lbdConfig . lcLambdas


getLambdaPermissionResourceName
  :: Lambda
  -> Text
getLambdaPermissionResourceName lbd =  T.concat [lbd ^. lbdName, "LambdaPermission"]

