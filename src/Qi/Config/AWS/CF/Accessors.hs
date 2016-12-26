{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.CF.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Qi.Config.AWS
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.Lambda           (Lambda, lbdName, lcLambdas)
import qualified Qi.Config.AWS.Lambda.Accessors as Lambda
import           Qi.Config.Identifier


insert
  :: Custom
  -> (CustomId, (CfConfig -> CfConfig))
insert custom = (cid, insertIdToCustom)
  where
    insertIdToCustom = cfcCustoms %~ SHM.insert cid custom
    cid = CustomId $ hash custom

