{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.DDB (toResources) where

import           Control.Lens
import           Data.Aeson                  (Value (Array), object)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Stratosphere                hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.DDB           hiding (DdbAttrType (..))
import qualified Qi.Config.AWS.DDB           as DDB
import           Qi.Config.AWS.DDB.Accessors


toResources config = Resources . map toDdbTableRes $ getAll config
  where
    toDdbTableRes table =
      resource resName $
        DynamoDBTableProperties $
        dynamoDBTable
          attributeDefinitions
          keySchema
          provisionedThroughput
        & ddbtTableName ?~ (Literal tableName)

      where
        resName = getLogicalName table
        tableName = getPhysicalName table config


        attributeDefinitions = [
            dynamoDBTableAttributeDefinition
              (Literal $ table^.dtHashAttrDef.daName)
              (Literal . toAttrType $ table^.dtHashAttrDef.daType)
          ]
        keySchema = [
            dynamoDBTableKeySchema (Literal $ table^.dtHashAttrDef.daName) $ Literal HASH
          ]
        provisionedThroughput =
          dynamoDBTableProvisionedThroughput
            (Literal . Integer' $ provCap^.dpcRead)
            (Literal . Integer' $ provCap^.dpcWrite)
          where
            provCap = table^.dtProfile.dtpProvCap

        toAttrType DDB.S = S
        toAttrType DDB.N = N
        toAttrType DDB.B = B
