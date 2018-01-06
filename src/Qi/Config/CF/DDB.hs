{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.DDB (toResources) where

import           Control.Lens
import           Data.Aeson           (Value (Array), object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as SHM
import qualified Data.Text            as T
import           Protolude            hiding (getAll)
import           Stratosphere         hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.DDB    hiding (DdbAttrType (..))
import qualified Qi.Config.AWS.DDB    as DDB


toResources config = Resources . foldMap toDdbTableResources $ getAll config
  where
    toDdbTableResources table = [tableRes table] ++ toConditionalDdbTableResources table

    toConditionalDdbTableResources table =
      case table^.dtStreamHandler of
        Just lbdId -> [tableStreamEventSourceMappingRes table lbdId]
        _          -> []

    tableRes table =
      resource lname $
        DynamoDBTableProperties $
        dynamoDBTable
          keySchema
          provisionedThroughput
          & ddbtTableName ?~ (Literal pname)
          & ddbtStreamSpecification .~ streamSpec
          & ddbtAttributeDefinitions ?~ attributeDefinitions

      where
        streamSpec = const (dynamoDBTableStreamSpecification $ Literal NEW_AND_OLD_IMAGES)
          <$> (table^.dtStreamHandler)

        lname = getLogicalName config table
        pname = getPhysicalName config table


        attributeDefinitions = [
            dynamoDBTableAttributeDefinition
              (Literal $ table^.dtHashAttrDef.daName)
              (Literal . toAttrType $ table^.dtHashAttrDef.daType)
          ] <> case table^.dtProfile.dtpRangeKey of
                Just rangeKey ->
                  [dynamoDBTableAttributeDefinition
                    (Literal $ rangeKey^.daName)
                    (Literal . toAttrType $ rangeKey^.daType)
                  ]
                Nothing ->
                  []

        keySchema = [
            dynamoDBTableKeySchema (Literal $ table^.dtHashAttrDef.daName) $ Literal HASH
          ] <> case table^.dtProfile.dtpRangeKey of
                Just rangeKey ->
                  [dynamoDBTableKeySchema (Literal $ rangeKey^.daName) $ Literal RANGE]
                Nothing ->
                  []

        provisionedThroughput =
          dynamoDBTableProvisionedThroughput
            (Literal $ provCap^.dpcRead)
            (Literal $ provCap^.dpcWrite)
          where
            provCap = table^.dtProfile.dtpProvCap

        toAttrType DDB.S = S
        toAttrType DDB.N = N
        toAttrType DDB.B = B


    tableStreamEventSourceMappingRes table lbdId =
      resource lname $
        LambdaEventSourceMappingProperties $
        lambdaEventSourceMapping
        tableStreamArn
        (Literal lbdPName)
        "LATEST"

      where
        lname = tableLName `T.append` "EventSourceMapping"
        tableLName = getLogicalName config table
        tableStreamArn = GetAtt tableLName "StreamArn"
        lbdPName = getPhysicalName config $ getById config lbdId


