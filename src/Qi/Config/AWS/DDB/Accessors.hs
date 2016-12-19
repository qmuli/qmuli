{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.DDB.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.Identifier



getDdbTableLogicalName
  :: DdbTable
  -> Text
getDdbTableLogicalName table =  T.concat [table^.dtName, "DynamoDBTable"]

getFullDdbTableName
  :: DdbTable
  -> Config
  -> Text
getFullDdbTableName table config =
  (table^.dtName) `underscoreNamePrefixWith` config


getAllDdbTables
  :: Config
  -> [DdbTable]
getAllDdbTables config = SHM.elems $ config^.ddbConfig.dcTables


getDdbTableById
  :: DdbTableId
  -> Config
  -> DdbTable
getDdbTableById tid config =
  case SHM.lookup tid tableMap of
    Just lbd -> lbd
    Nothing  -> error $ "Could not reference s3 bucket with id: " ++ show tid
  where
    tableMap = config^.ddbConfig.dcTables

