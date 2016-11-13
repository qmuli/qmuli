{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.DDB (toResources) where

import           Data.Aeson                  (Value (Array), object)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Stratosphere                hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.DDB.Accessors


toResources config = Resources . map toDdbTableRes $ getAllDdbTables config
  where
    toDdbTableRes table =
      resource name $
        DynamoDBTableProperties $
        dynamoDBTable
          attributeDefinitions
          keySchema
          provisionedThroughput
        & ddbtTableName ?~ (Literal $ table^.dtName)

      where
        name = getDdbTableCFResourceName table

        attributeDefinitions = [
            dynamoDBAttributeDefinition
              (Literal $ table^.dtHashAttrDef.daName)
              (Literal . T.pack . show $ table^.dtHashAttrDef.daType)
          ]
        keySchema = [
            dynamoDBKeySchema (Literal $ table^.dtHashAttrDef.daName) "HASH"
          ]
        provisionedThroughput =
          dynamoDBProvisionedThroughput
            (Literal . Integer' $ table^.dtProvCap.dpcRead)
            (Literal . Integer' $ table^.dtProvCap.dpcWrite)


