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


getLogicalName
  :: Custom
  -> Config
  -> Text
getLogicalName custom config =
  T.concat [ Lambda.getLogicalNameFromId (custom ^. cLbdId) config, "CustomResource"]


getById
  :: CustomId
  -> Config
  -> Custom
getById cid config =
  case SHM.lookup cid customMap of
    Just custom -> custom
    Nothing  -> error $ "Could not reference s3 bucket with id: " ++ show cid
  where
    customMap = config^.cfConfig.cfcCustoms


getAll
  :: Config
  -> [Custom]
getAll config = SHM.elems $ config^.cfConfig.cfcCustoms



insert
  :: Custom
  -> (CustomId, (CfConfig -> CfConfig))
insert custom = (cid, insertIdToCustom)
  where
    insertIdToCustom = cfcCustoms %~ SHM.insert cid custom
    cid = CustomId $ hash custom

