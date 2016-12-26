{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.CW.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.CW
import           Qi.Config.Identifier


rType = "events rule"
getName = (^.cerName)
getMap = (^.cwConfig.ccRules)


getLogicalName
  :: CwEventsRule
  -> Text
getLogicalName r =  T.concat [getName r, "CwEventsRule"]

getPhysicalName
  :: CwEventsRule
  -> Config
  -> Text
getPhysicalName r config =
  getName r `underscoreNamePrefixWith` config

getAll
  :: Config
  -> [CwEventsRule]
getAll = SHM.elems . getMap

getById
  :: CwEventsRuleId
  -> Config
  -> CwEventsRule
getById rid config =
  fromMaybe
    (error $ "Could not reference " ++ rType ++ " with id: " ++ show rid)
    $ SHM.lookup rid $ getMap config


