{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.DDB.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.Identifier



getDdbTableCFResourceName
  :: DdbTable
  -> Text
getDdbTableCFResourceName table =  T.concat [table^.dtName, "DynamoDBTable"]


getAllDdbTables
  :: Config
  -> [DdbTable]
getAllDdbTables config = SHM.elems $ config^.ddbConfig.dcTables


getDdbTableById
  :: DdbTableId
  -> Config
  -> DdbTable
getDdbTableById tid = fromJust . SHM.lookup tid . (^.ddbConfig.dcTables)


insertDdbTable
  :: DdbTable
  -> (DdbTableId, (DdbConfig -> DdbConfig))
insertDdbTable table = (tid, insertIdToDdbTable)
  where
    insertIdToDdbTable = dcTables %~ SHM.insert tid table
    tid = DdbTableId $ hash table
