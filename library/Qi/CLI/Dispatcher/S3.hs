{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.CLI.Dispatcher.S3 where

import           Control.Lens
import           Control.Monad.Freer    hiding (send)
import           Protolude              hiding (getAll)
import           Qi.Config.AWS          (Config, getAll, getAllWithIds, getById,
                                         getPhysicalName, namePrefix)
import           Qi.Config.AWS.S3       (S3Bucket, s3bName)
import           Qi.Config.Identifier   (S3BucketId)
import           Qi.Program.Config.Lang
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang


clearBuckets
  :: Members '[ S3Eff, GenEff, ConfigEff ] effs
  => Eff effs ()
clearBuckets  = do
  config <- getConfig
  let bucketIds = map fst $ getAllWithIds config
  say "destroying buckets..."
  for_ bucketIds $ \bucketId -> do
    say $ "destroying bucket: '" <> (getById config bucketId) ^. s3bName <> "'"
    forAll bucketId deleteObjects

  where
    forAll bucketId action = go bucketId action Nothing False

    go _ _ Nothing True = pass
    go bucketId action maybeToken _ = do
      (objs, maybeToken') <- listObjects bucketId maybeToken
      action objs
      go bucketId action maybeToken' True


