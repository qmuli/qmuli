{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.Gen.Lang where

import           Control.Monad.Freer
import qualified Control.Monad.Trans.AWS              as AWS (send)
import           Data.Aeson                           (FromJSON, ToJSON, Value)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Time.Clock                      (UTCTime)
import           Network.AWS                          hiding (Request, Response,
                                                       send)
import           Network.AWS.Data.Body                (RsBody (..))
import           Network.HTTP.Client
import           Protolude
import           Qi.Config.AWS.CfCustomResource.Types (CfCustomResourceEvent)
import           Qi.Core.Curry
import           Servant.Client                       (BaseUrl, ClientM,
                                                       ServantError)


{- type CfCustomResourceLambdaProgram effs = CfCustomResourceEvent -> Eff effs LBS.ByteString -}

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
    => Service
    -> a
    -> GenEff (Rs a)

  AmazonkaPostBodyExtract
    :: (AWSRequest a)
    => Service
    -> a
    -> (Rs a -> RsBody)
    -> GenEff (Either Text LBS.ByteString)

  Say
    :: Text
    -> GenEff ()

  GetCurrentTime
    :: GenEff UTCTime

  Sleep
    :: Int
    -> GenEff ()

  Build
    :: GenEff Text -- TODO return status

  ReadFileLazy
    :: Text
    -> GenEff LBS.ByteString

  GetLine
    :: GenEff BS.ByteString

  PutStr
    :: LBS.ByteString
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
  => Service
  -> a
  -> Eff effs (Rs a)
amazonka =
  send .: Amazonka


amazonkaPostBodyExtract
  :: (AWSRequest a, Member GenEff effs)
  => Service
  -> a
  -> (Rs a -> RsBody)
  -> Eff effs (Either Text LBS.ByteString)
amazonkaPostBodyExtract =
  send .:: AmazonkaPostBodyExtract


getCurrentTime
  :: Member GenEff effs
  => Eff effs UTCTime
getCurrentTime =
  send GetCurrentTime


sleep
  :: Member GenEff effs
  => Int
  -> Eff effs ()
sleep = send . Sleep


say
  :: Member GenEff effs
  => Text
  -> Eff effs ()
say = send . Say

build
  :: Member GenEff effs
  => Eff effs Text
build = send Build

readFileLazy
    :: Member GenEff effs
    => Text
    -> Eff effs LBS.ByteString
readFileLazy = send . ReadFileLazy

getLine
  :: Member GenEff effs
  => Eff effs BS.ByteString
getLine = send GetLine

putStr
  :: Member GenEff effs
  => LBS.ByteString
  -> Eff effs ()
putStr = send . PutStr


