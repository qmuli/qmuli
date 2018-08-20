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
import           Qi.Config.Identifier (LambdaId)
import           Qi.Core.Curry


data LambdaEff r where
  InvokeLambda
    :: ToJSON a
    => LambdaId
    -> a
    -> LambdaEff ()

invokeLambda
  :: (Member LambdaEff effs, ToJSON a)
  => LambdaId
  -> a
  -> Eff effs ()
invokeLambda =
  send .: InvokeLambda

