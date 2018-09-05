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

module Qi.Test.CLI.Deploy where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Data.Default                  (Default, def)
import qualified Data.HashMap.Strict           as SHM
{- import           Network.AWS.CloudFormation -}
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Protolude                     hiding (Reader, State, get, put,
                                                runReader, runState)
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
  }
  deriving (Eq, Show)

instance Semigroup Journal where
  Journal as1 s31 <> Journal as2 s32 = Journal (as1 <> as2) (s31 <> s32)

instance Monoid Journal where
  mempty = Journal mempty mempty

instance Default Journal where
  def = mempty

data CfAction =
    CreateStackAction StackName S3Object
  | UpdateStackAction StackName S3Object
  | DeleteStackAction StackName
  deriving (Eq, Show)


data S3Action =
    CreateBucketAction Text
  | PutContentAction S3Object LBS.ByteString
  | DeleteBucketAction Text
  deriving (Eq, Show)


data Params = Params


spec :: Spec
spec = parallel $
  describe "Deploy" $ do
    let params = Params
        config = def :: Config

    it "works" $ do
      let expected = def{
              s3Actions = [ CreateBucketAction "qmuli"
                          , PutContentAction (S3Object {_s3oBucketId = S3BucketId 1, _s3oKey = S3Key "cf.json"}) "rendered template"
                          , PutContentAction (S3Object {_s3oBucketId = S3BucketId 1, _s3oKey = S3Key "lambda.zip"}) "dummy binary"
                          ]
            }
          result :: Journal =
              run
            . map snd . runWriter
            . runReader params
            . map fst . runState config
            . Config.run
            . testGenRun
            . Lbd.run
            . testS3Run
            . testCfRun
            $ deployApp "rendered template" "dummy binary"

      result `shouldBe` expected




testGenRun
  :: forall effs a
  .  (Members '[ ConfigEff ] effs)
  => (Eff (GenEff ': effs) a -> Eff effs a)
testGenRun = interpret (\case

  GetAppName ->
    (^. namePrefix) <$> getConfig

  Http mgrSettings _req ->
    panic "Http"

  RunServant _mgrSettings _baseUrl _req ->
    panic "RunServant"

  {- Amazonka cs@CreateStack{} -> do -}
    {- send . tell $ AmzCreateStack cs -}
    {- pure $ createStackResponse 201 -}

  Amazonka _req ->
    panic "Amazonka"

  AmazonkaPostBodyExtract _req _post ->
    panic "AmazonkaPostBodyExtract"

  Say _msg ->
    pass
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

  CreateStack name s3Obj -> do
    tell $ cfAction $ CreateStackAction name s3Obj

  UpdateStack name s3Obj -> do
    tell $ cfAction $ UpdateStackAction name s3Obj

  DeleteStack name ->
    panic "DeleteStack"

  DescribeStacks ->
    panic "DescribeStacks"

  WaitOnStackStatus name status' isAbsentOk -> do
    panic "DescribeStacks"

  )


testS3Run
  :: forall effs a
  .  (Members '[ ConfigEff, Reader Params, Writer Journal ] effs)
  => (Eff (S3Eff ': effs) a -> Eff effs a)
testS3Run = interpret (\case

  CreateBucket name -> do
    tell $ s3Action $ CreateBucketAction name
    pure $ S3BucketId 1

  GetContent S3Object{ _s3oBucketId, _s3oKey } ->
    panic "GetContent"


  PutContent s3Obj@S3Object{_s3oBucketId, _s3oKey } payload -> do
    tell $ s3Action $ PutContentAction s3Obj payload
    {- panic "PutContent" -}


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


cfAction
  :: CfAction
  -> Journal
cfAction action = def{ cfActions = [action]}

s3Action
  :: S3Action
  -> Journal
s3Action action = def{ s3Actions = [action]}

