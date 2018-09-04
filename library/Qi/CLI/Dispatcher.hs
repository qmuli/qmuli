{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.CLI.Dispatcher (
    updateLambdas
  , deployApp
  , createCfStack
  , updateCfStack
  , describeCfStack
  , destroyCfStack
  , cycleStack
  ) where

import           Control.Lens
import           Control.Monad.Freer
import           Data.Aeson.Encode.Pretty       (encodePretty)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import           Protolude                      hiding (FilePath, getAll)
import           Qi.CLI.Dispatcher.S3           as S3 (clearBuckets)
import           Qi.Config.AWS                  (Config, getAll,
                                                 getPhysicalName, namePrefix)
import           Qi.Config.AWS.Lambda           (Lambda)
import qualified Qi.Config.AWS.Lambda.Accessors as Lbd
import           Qi.Config.AWS.S3               (S3Key (S3Key), s3Object)
import qualified Qi.Config.AWS.S3.Accessors     as S3
import qualified Qi.Config.CfTemplate           as CF
import           Qi.Program.CF.Lang
import           Qi.Program.Config.Lang         (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.Lambda.Lang         (LambdaEff)
import qualified Qi.Program.Lambda.Lang         as Lbd
import           Qi.Program.S3.Lang



deployApp
  :: Members '[ S3Eff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> Eff effs ()
deployApp content = do
  config <- getConfig
  let appName = config ^. namePrefix

  say "deploying the app..."
  bucketId <- createBucket appName
  putContent (s3Object bucketId $ S3Key "cf.json") $ CF.render config -- TODO: render this inside docker container: https://github.com/qmuli/qmuli/issues/60
  putContent (s3Object bucketId $ S3Key "lambda.zip") content



createCfStack
  :: Members '[ CfEff, GenEff, ConfigEff ] effs
  => Eff effs ()
createCfStack = do
  config <- getConfig
  let name = config ^. namePrefix
      stackName   = StackName name
      stackS3Obj  = s3Object (S3.getIdByName config name) $ S3Key "cf.json"

  say "creating the stack..."
  createStack stackName stackS3Obj

  say "waiting on the stack to be created..."
  waitOnStackStatus stackName SSCreateComplete NoAbsent

  say "stack was successfully created"


updateCfStack
  :: Members '[ LambdaEff, CfEff, GenEff, ConfigEff ] effs
  => Eff effs ()
updateCfStack = do
  config <- getConfig
  let name = config ^. namePrefix
      stackName   = StackName name
      stackS3Obj  = s3Object (S3.getIdByName config name) $ S3Key "cf.json"

  say "updating the stack..."
  updateStack stackName stackS3Obj

  say "waiting on the stack to be updated..."
  waitOnStackStatus stackName SSUpdateComplete NoAbsent

  -- TODO: make lambda updating concurrent with the above stack update?
  updateLambdas

  say "stack was successfully updated"


updateLambdas
  :: Members '[ LambdaEff, GenEff, ConfigEff ] effs
  => Eff effs ()
updateLambdas = do
  config <- getConfig
  let name      = config ^. namePrefix
      lbdS3Obj  = s3Object (S3.getIdByName config name) $ S3Key "lambda.zip"

  say "updating the lambdas..."
  traverse_ ((`Lbd.update` lbdS3Obj) . Lbd.getIdByName config . getPhysicalName config)
    (getAll config :: [ Lambda ])


describeCfStack
  :: Members '[ CfEff, GenEff, ConfigEff ] effs
  => Eff effs ()
describeCfStack = do
  config <- getConfig
  let stackName = StackName $ config ^. namePrefix
  stackDict <- describeStacks
  maybe (panic $ "stack '" <> show stackName <> "' not found") (say . toS . encodePretty) $ Map.lookup stackName stackDict


destroyCfStack
  :: Members '[ LambdaEff, CfEff, S3Eff, GenEff, ConfigEff ] effs
  => Eff effs ()
  -> Eff effs ()
destroyCfStack action = do
  config <- getConfig
  let stackName = StackName $ config ^. namePrefix

  say "destroying the stack..."

  clearBuckets
  deleteStack stackName

  action

  say "waiting on the stack to be destroyed..."
  waitOnStackStatus stackName SSDeleteComplete AbsentOk

  say "stack was successfully destroyed"


cycleStack
  :: Members '[ LambdaEff, CfEff, S3Eff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> Eff effs ()
cycleStack content = do
  destroyCfStack $ deployApp content
  createCfStack
  say "all done!"




