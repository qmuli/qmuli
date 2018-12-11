{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.Render.SQS (toResources) where

import           Control.Lens
import           Protolude         hiding (getAll)
import           Qi.Config.AWS
import           Qi.Config.AWS.SQS
import           Stratosphere      (ResourceProperties (SQSQueueProperties),
                                    Resources (Resources),
                                    Val (GetAtt, Literal))
import qualified Stratosphere      as S


toResources
  :: Config
  -> Resources
toResources config = foldMap toAllSqsResources $ getAll config
  where
    toAllSqsResources :: SqsQueue -> Resources
    toAllSqsResources sqsQueue = Resources $ [queueResource]

      where
        queueResource = (
          S.resource (unLogicalName $ getLogicalName config sqsQueue) $
            SQSQueueProperties $
              S.sqsQueue
              & S.sqsqQueueName ?~ Literal (sqsQueue ^. sqsQueueName)
          )
