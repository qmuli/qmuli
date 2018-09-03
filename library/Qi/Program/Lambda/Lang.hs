{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.Lambda.Lang where

import           Control.Monad.Freer
import           Data.Aeson           (ToJSON)
import           Protolude
import           Qi.Config.AWS.S3     (S3Object)
import           Qi.Config.Identifier (LambdaId)
import           Qi.Core.Curry


data LambdaEff r where

  Invoke
    :: ToJSON a
    => LambdaId
    -> a
    -> LambdaEff ()

  Update
    :: LambdaId
    -> S3Object
    -> LambdaEff ()


invoke
  :: (Member LambdaEff effs, ToJSON a)
  => LambdaId
  -> a
  -> Eff effs ()
invoke =
  send .: Invoke


update
  :: (Member LambdaEff effs)
  => LambdaId
  -> S3Object
  -> Eff effs ()
update =
  send .: Update

