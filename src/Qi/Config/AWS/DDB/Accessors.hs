{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.DDB.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.Identifier


getLogicalName
  :: DdbTable
  -> Text
getLogicalName table =  T.concat [table^.dtName, "DynamoDBTable"]


getPhysicalName
  :: DdbTable
  -> Config
  -> Text
getPhysicalName table config =
  (table^.dtName) `underscoreNamePrefixWith` config


getAll
  :: Config
  -> [DdbTable]
getAll config = SHM.elems $ config^.ddbConfig.dcTables


getById
  :: DdbTableId
  -> Config
  -> DdbTable
getById tid config =
  case SHM.lookup tid tableMap of
    Just lbd -> lbd
    Nothing  -> error $ "Could not reference s3 bucket with id: " ++ show tid
  where
    tableMap = config^.ddbConfig.dcTables

