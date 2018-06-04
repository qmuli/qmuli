{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.SQS (toResources) where

import           Protolude     hiding (getAll)
import           Qi.Config.AWS (Config, getAll, getById, getLogicalName,
                                getLogicalNameFromId, getPhysicalName)
import           Stratosphere  (ResourceProperties (S3BucketProperties),
                                Resources (Resources), Val (GetAtt, Literal))


toResources
  :: Config
  -> Resources
toResources config = foldMap toAllSqsResources $ getAll config
  where
    toAllSqsResources :: SqsQueue -> Resources
    toAllSqsResources sqsQueue = Resources $ [queueResource]

      where
        qln = getLogicalName config sqsQueue

        queueResource = (
          resource qln $
            LambdaFunctionProperties $
            lambdaFunction
              lbdCode
              "index.handler"
              (GetAtt Role.lambdaBasicExecutionIAMRoleLogicalName "Arn")
              (Literal NodeJS43)
            & lfFunctionName ?~ Literal lambdaName
            & lfMemorySize ?~ Literal memorySize
            & lfTimeout ?~ Literal timeOut
          )
