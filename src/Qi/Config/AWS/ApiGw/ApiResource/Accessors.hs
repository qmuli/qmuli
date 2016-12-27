{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.ApiGw.ApiResource.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.Identifier


getChildren
  :: Either ApiId ApiResourceId
  -> Config
  -> [ApiResourceId]
getChildren rid config = SHM.lookupDefault [] rid $ config^.apiGwConfig.acApiResourceDeps




