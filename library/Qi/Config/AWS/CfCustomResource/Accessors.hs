{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.CfCustomResource.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict            as SHM
import qualified Data.Text                      as T
import           Protolude

import           Qi.Config.AWS
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.Lambda           (Lambda, lbdName, lcLambdas)
import qualified Qi.Config.AWS.Lambda.Accessors as Lambda
import           Qi.Config.Identifier


insert
  :: CfCustomResource
  -> (CfCustomResourceId, (CfConfig -> CfConfig))
insert customResource = (customResourceId, insertIdToCustom)
  where
    insertIdToCustom = cfcCustomResources %~ SHM.insert customResourceId customResource
    customResourceId = CfCustomResourceId $ hash customResource

