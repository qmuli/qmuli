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
  , printLogs
  ) where

import           Control.Lens
import           Control.Monad.Freer
import           Data.Aeson.Encode.Pretty       (encodePretty)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import           Network.AWS.CloudWatchLogs
import           Network.AWS.Data.Body          (toBody)
import           Network.AWS.S3
import           Protolude                      hiding (FilePath, getAll)
import           Qi.CLI.Dispatcher.S3           as S3 (clearBuckets)
import           Qi.Config.AWS                  (Config, getAll, getName,
                                                 getPhysicalName, namePrefix)
import           Qi.Config.AWS                  (getById)
import           Qi.Config.AWS.Lambda           (Lambda)
import qualified Qi.Config.AWS.Lambda.Accessors as Lbd
import           Qi.Config.AWS.S3               (S3Key (S3Key), s3Object)
import qualified Qi.Config.AWS.S3.Accessors     as S3
import qualified Qi.Config.CfTemplate           as CF
import           Qi.Config.Identifier           (LambdaId)
import           Qi.Program.CF.Lang
import           Qi.Program.Config.Lang         (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.Lambda.Lang         (LambdaEff)
import qualified Qi.Program.Lambda.Lang         as Lbd
import           Qi.Program.S3.Lang


deployApp
  :: Members '[ S3Eff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> LBS.ByteString
  -> Eff effs ()
deployApp template content = do
  say "deploying the app..."
  config <- getConfig
  let appName     = config ^. namePrefix
      bucketName = BucketName $ appName <> ".app"

  say $ "creating bucket '" <> show bucketName <> "'"
  amazonka s3 $ createBucket bucketName

  say $ "writing lambda executable into bucket '" <> show bucketName <> "'"
  amazonka s3 $ putObject bucketName "lambda.zip" (toBody content) & poACL ?~ OPublicReadWrite
  pass

createCfStack
  :: Members '[ CfEff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> Eff effs ()
createCfStack template = do
  config <- getConfig
  let appName     = config ^. namePrefix
      stackName   = StackName appName

  say "creating the stack..."
  createStack stackName template

  say "waiting on the stack to be created..."
  waitOnStackStatus stackName SSCreateComplete NoAbsent

  say "stack was successfully created"


updateCfStack
  :: Members '[ LambdaEff, CfEff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> Eff effs ()
updateCfStack template = do
  config <- getConfig
  let appName     = config ^. namePrefix
      stackName   = StackName appName

  say "updating the stack..."
  updateStack stackName template

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
  let lbdS3Obj    = s3Object (S3.getIdByName config "app") $ S3Key "lambda.zip"

  say "updating the lambdas..."
  traverse_ ((`Lbd.update` lbdS3Obj) . Lbd.getIdByName config . getName config)
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
  -> LBS.ByteString
  -> Eff effs ()
cycleStack template content = do
  destroyCfStack $ deployApp template content
  createCfStack template
  say "all done!"



printLogs
  :: Members '[ GenEff, ConfigEff ] effs
  => Text
  -> Eff effs ()
printLogs lbdName = do
  config <- getConfig
  let lbdId = Lbd.getIdByName config lbdName
      lbd   = getById config lbdId :: Lambda
      groupName = "/aws/lambda/" <> show (getPhysicalName config lbd)
  res <- amazonka cloudWatchLogs $ filterLogEvents groupName
  traverse_ say . catMaybes $ (^.fleMessage) <$> res ^. flersEvents

