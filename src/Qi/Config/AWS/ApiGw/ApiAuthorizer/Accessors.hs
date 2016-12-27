{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.ApiGw.ApiAuthorizer.Accessors where

import           Control.Lens
import           Data.Char            (isAlphaNum)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.Identifier


getChildren
  :: ApiId
  -> Config
  -> [ApiAuthorizerId]
getChildren aid config =
  SHM.lookupDefault [] aid $ config^.apiGwConfig.acApiAuthorizerDeps



