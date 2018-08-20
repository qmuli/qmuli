{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.SQS.Lang where

import           Control.Monad.Freer
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import           Network.AWS.SQS
import           Protolude
import           Qi.AWS.SQS
import           Qi.Config.AWS.SQS
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Qi.Program.Gen.Lang


data SqsEff r where
  SendSqsMessage
    :: ToJSON a
    => SqsQueueId
    -> a
    -> SqsEff ()

  ReceiveSqsMessage
    :: FromJSON a
    => SqsQueueId
    -> SqsEff [(a, ReceiptHandle)]

  DeleteSqsMessage
    :: SqsQueueId
    -> ReceiptHandle
    -> SqsEff ()


sendSqsMessage
  :: (Member SqsEff effs, ToJSON a)
  => SqsQueueId
  -> a
  -> Eff effs ()
sendSqsMessage = send .: SendSqsMessage

receiveSqsMessage
  :: (Member SqsEff effs, FromJSON a)
  => SqsQueueId
  -> Eff effs [(a, ReceiptHandle)] -- the json body and the receipt handle
receiveSqsMessage = send . ReceiveSqsMessage

deleteSqsMessage
  :: (Member SqsEff effs)
  => SqsQueueId
  -> ReceiptHandle
  -> Eff effs ()
deleteSqsMessage = send .: DeleteSqsMessage


