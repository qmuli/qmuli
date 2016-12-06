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
import           Qi.Config.AWS.Lambda.Accessors (getLambdaCFResourceNameFromId)
import           Qi.Config.Identifier


getCustomCFResourceName
  :: Custom
  -> Config
  -> Text
getCustomCFResourceName custom config =
  T.concat [ getLambdaCFResourceNameFromId (custom ^. cLbdId) config, "CustomResource"]


getCustomById
  :: CustomId
  -> Config
  -> Custom
getCustomById cid config =
  case SHM.lookup cid customMap of
    Just custom -> custom
    Nothing  -> error $ "Could not reference s3 bucket with id: " ++ show cid
  where
    customMap = config^.cfConfig.cfcCustoms


getAllCustoms
  :: Config
  -> [Custom]
getAllCustoms config = SHM.elems $ config^.cfConfig.cfcCustoms



insertCustom
  :: Custom
  -> (CustomId, (CfConfig -> CfConfig))
insertCustom custom = (cid, insertIdToCustom)
  where
    insertIdToCustom = cfcCustoms %~ SHM.insert cid custom
    cid = CustomId $ hash custom

