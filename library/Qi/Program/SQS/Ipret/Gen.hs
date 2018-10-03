{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.SQS.Ipret.Gen  where

import           Control.Lens         (Getting, (.~), (?~), (^.))
import           Control.Monad.Freer
import           Data.Aeson           (decode, encode)
import           Network.AWS.SQS
import           Protolude            hiding (handle, (<&>))
import           Qi.AWS.SQS
import           Qi.Config.AWS
import           Qi.Config.Identifier (LambdaId)
import           Qi.Program.Gen.Lang
import           Qi.Program.SQS.Lang


run
  :: forall effs a
  .  (Member GenEff effs)
  => Config
  -> (Eff (SqsEff ': effs) a -> Eff effs a)
run config = interpret (\case

  SendSqsMessage id msg ->
    void . amazonka . sendMessage queueUrl . toS $ encode msg
    where
      queueUrl = getPhysicalName config $ getById config id


  ReceiveSqsMessage id -> do
      r <- amazonka $ receiveMessage queueUrl
      pure . catMaybes $ (\m -> do
                            msg <- decode . toS =<< m ^. mBody
                            rh <- m ^. mReceiptHandle
                            pure (msg, ReceiptHandle rh)
                          ) <$> (r ^. rmrsMessages)

      where
        queueUrl = getPhysicalName config $ getById config id


  DeleteSqsMessage id handle -> panic "deleteSqsMessage unimplemented"

  )
