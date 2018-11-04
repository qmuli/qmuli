{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Test.Ipret where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Default                  (Default, def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude                     hiding (Reader, State, asks, get,
                                                log, put, runReader, runState)
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import qualified Qi.Program.CF.Ipret.Gen       as CF
import           Qi.Program.CF.Lang
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang
import           Qi.Program.Gen.Lang
import qualified Qi.Program.Lambda.Ipret.Gen   as Lbd
import qualified Qi.Program.S3.Ipret.Gen       as S3
import           Qi.Program.S3.Lang
import qualified Qi.Program.Wiring.IO          as IO
import           Test.Tasty.Hspec


data Journal = Journal {
    cfActions :: [ CfAction ]
  , s3Actions :: [ S3Action ]
  , logs      :: [ Text ]
  }
  deriving (Eq, Show)

instance Semigroup Journal where
  Journal cf1 s31 ls1 <> Journal cf2 s32 ls2 =
    Journal (cf1 <> cf2) (s31 <> s32) (ls1 <> ls2)

instance Monoid Journal where
  mempty = Journal mempty mempty mempty

instance Default Journal where
  def = mempty

data CfAction =
    CreateStackAction StackName LBS.ByteString
  | UpdateStackAction StackName LBS.ByteString
  | DeleteStackAction StackName
  | DescribeStacksAction
  deriving (Eq, Show)


data S3Action =
    PutContentAction S3Object LBS.ByteString
  | DeleteBucketAction Text
  deriving (Eq, Show)


data Params = Params {
    config :: Config
  , stacks :: StackDescriptionDict
  }

instance Default Params where
  def = Params {
            config = def
          , stacks = mempty
          }


testGenRun
  :: forall effs a
  .  (Members '[ ConfigEff, Writer Journal ] effs)
  => (Eff (GenEff ': effs) a -> Eff effs a)
testGenRun = interpret (\case

  GetAppName ->
    (^. namePrefix) <$> getConfig

  Http mgrSettings _req ->
    panic "Http"

  RunServant _mgrSettings _baseUrl _req ->
    panic "RunServant"

  Amazonka _svc _req ->
    panic "Amazonka"

  AmazonkaPostBodyExtract _svc _req _post ->
    panic "AmazonkaPostBodyExtract"

  Say msg ->
    log msg
    -- panic "Say"

  GetCurrentTime ->
    panic "GetCurrentTime"

  Sleep _us ->
    panic "Sleep"

  Build ->
    panic "Build"

  ReadFileLazy _path ->
    panic "ReadFileLazy"

  GetLine ->
    panic "GetLine"

  PutStr _content ->
    panic "PutStr"

  )


testCfRun
  :: forall effs a
  .  (Members '[ ConfigEff, Reader Params, Writer Journal ] effs)
  => (Eff (CfEff ': effs) a -> Eff effs a)
testCfRun = interpret (\case

  CreateStack name template -> do
    cfAction $ CreateStackAction name template

  UpdateStack name template -> do
    cfAction $ UpdateStackAction name template

  DeleteStack name ->
    panic "DeleteStack"

  DescribeStacks -> do
    cfAction $ DescribeStacksAction
    asks stacks

  WaitOnStackStatus name status isAbsentOk -> do
    -- asks stackdescs
    pure ()

  )


testS3Run
  :: forall effs a
  .  (Members '[ ConfigEff, Reader Params, Writer Journal ] effs)
  => (Eff (S3Eff ': effs) a -> Eff effs a)
testS3Run = interpret (\case

  GetContent S3Object{ _s3oBucketId, _s3oKey } ->
    panic "GetContent"


  PutContent s3Obj@S3Object{_s3oBucketId, _s3oKey } payload -> do
    s3Action $ PutContentAction s3Obj payload

  ListObjects id maybeToken -> do
    panic "ListObjects"


  DeleteObject s3Obj@S3Object{_s3oBucketId, _s3oKey } -> do
    {- config <- getConfig -}
    {- getName _s3oBucketId -}
    {- tell $ s3Action $ DeleteBucketAction name -}
    panic "DeleteObject"


  DeleteObjects s3objs -> do
    panic "DeleteObjects"

  )

testRun
  :: Params
  -> Eff '[ CfEff, S3Eff, GenEff, ConfigEff, State Config, Reader Params, Writer Journal ] ()
  -> (Config, Journal)
testRun params@Params{ config } = run
  . runWriter
  . runReader params
  . map snd . runState config
  . Config.run
  . testGenRun
  {- . Lbd.run -}
  . testS3Run
  . testCfRun


cfAction
  :: Member (Writer Journal) effs
  => CfAction
  -> Eff effs ()
cfAction action = tell $ def{ cfActions = [action]}

s3Action
  :: Member (Writer Journal) effs
  => S3Action
  -> Eff effs ()
s3Action action = tell $ def{ s3Actions = [action]}

log
  :: Member (Writer Journal) effs
  => Text
  -> Eff effs ()
log msg = tell $ def{ logs = [msg]}


