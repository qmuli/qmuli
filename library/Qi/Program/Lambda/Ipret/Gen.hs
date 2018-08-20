{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.Lambda.Ipret.Gen  where

import           Control.Lens           (Getting, (.~), (?~), (^.))
import           Control.Monad.Freer
import           Data.Aeson             (encode)
import           Network.AWS.Lambda     (InvocationType (Event),
                                         iInvocationType, invoke)
import           Protolude              hiding ((<&>))
import           Qi.Config.AWS
import           Qi.Config.Identifier   (LambdaId)
import           Qi.Program.Gen.Lang
import           Qi.Program.Lambda.Lang


run
  :: forall effs a
  .  (Member GenEff effs)
  => Config
  -> (Eff (LambdaEff ': effs) a -> Eff effs a)
run config = interpret (\case

  InvokeLambda id payload ->
    void . amazonka $ invoke pname (toS $ encode payload) & iInvocationType ?~ Event
    where
      pname = getPhysicalName config $ getById config id

  )
