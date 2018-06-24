module Qi.AWS.SQS where

import           Protolude


newtype ReceiptHandle = ReceiptHandle Text
  deriving (Eq, Show)


