{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.Gen.Lang where

import           Control.Monad.Freer
import           Data.Aeson                           (FromJSON, ToJSON, Value)
import qualified Data.ByteString.Lazy                 as LBS
import           Network.AWS                          hiding (Request, Response,
                                                       send)
import           Network.AWS.Data.Body                (RsBody (..))
import           Network.HTTP.Client
import           Protolude
import           Qi.Config.AWS.CfCustomResource.Types (CfCustomResourceEvent)
import           Qi.Core.Curry
import           Servant.Client                       (BaseUrl, ClientM,
                                                       ServantError)



type CfCustomResourceLambdaProgram effs = CfCustomResourceEvent -> Eff effs LBS.ByteString

data GenEff r where
  GetAppName
    :: GenEff Text

  Http
    :: ManagerSettings
    -> Request
    -> GenEff (Response LBS.ByteString)

  RunServant
    :: ManagerSettings
    -> BaseUrl
    -> ClientM a
    -> GenEff (Either ServantError a)

  Amazonka
    :: (AWSRequest a)
    => a
    -> GenEff (Rs a)

  AmazonkaPostBodyExtract
    :: (AWSRequest a)
    => a
    -> (Rs a -> RsBody)
    -> GenEff (Either Text LBS.ByteString)

  Say
    :: Text
    -> GenEff ()


getAppName
  :: (Member GenEff effs)
  => Eff effs Text
getAppName =
  send GetAppName


http
  :: Member GenEff effs
  => ManagerSettings
  -> Request
  -> Eff effs (Response LBS.ByteString)
http =
  send .: Http


runServant
  :: Member GenEff effs
  => ManagerSettings
  -> BaseUrl
  -> ClientM a
  -> Eff effs (Either ServantError a)
runServant =
  send .:: RunServant


amazonka
  :: (AWSRequest a, Member GenEff effs)
  => a
  -> Eff effs (Rs a)
amazonka req =
  send $ Amazonka req

amazonkaPostBodyExtract
  :: (AWSRequest a, Member GenEff effs)
  => a
  -> (Rs a -> RsBody)
  -> Eff effs (Either Text LBS.ByteString)
amazonkaPostBodyExtract req post =
  send $ AmazonkaPostBodyExtract req post

{-
getCurrentTime
  :: LambdaProgram UTCTime
getCurrentTime = singleton GetCurrentTime

sleep
  :: Int
  -> LambdaProgram ()
sleep = singleton . Sleep
-}

say
  :: Member GenEff effs
  => Text
  -> Eff effs ()
say = send . Say


