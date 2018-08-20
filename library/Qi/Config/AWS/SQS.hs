{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.SQS where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           GHC.Show             (Show (..))
import           Protolude
import           Qi.Config.Identifier


data SqsQueue = SqsQueue {
    _sqsQueueName         :: Text
  }
  deriving (Eq, Show)

data SqsConfig = SqsConfig {
    _sqsQueues :: HashMap SqsQueueId SqsQueue
  }
  deriving (Eq, Show)

instance Default SqsConfig where
  def = SqsConfig {
    _sqsQueues = SHM.empty
  }

makeLenses ''SqsQueue
makeLenses ''SqsConfig

