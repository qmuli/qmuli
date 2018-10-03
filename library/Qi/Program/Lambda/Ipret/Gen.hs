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
import           Network.AWS.Lambda     (uS3Bucket, uS3Key, updateFunctionCode)
import           Network.AWS.S3         (ObjectKey (ObjectKey))
import           Protolude              hiding ((<&>))
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier   (LambdaId)
import           Qi.Program.Config.Lang (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.Lambda.Lang (LambdaEff (..))


run
  :: forall effs a
  .  (Member GenEff effs, Member ConfigEff effs)
  => (Eff (LambdaEff ': effs) a -> Eff effs a)
run = interpret (\case

  Invoke id payload -> do
    config  <- getConfig
    let pname = getPhysicalName config $ getById config id
    void . amazonka $ invoke pname (toS $ encode payload)
                        & iInvocationType ?~ Event


  Update id S3Object{ _s3oBucketId, _s3oKey = S3Key s3Key } -> do
    config  <- getConfig
    let pname = getPhysicalName config $ getById config id
        bucketName = getPhysicalName config $ getById config _s3oBucketId
    void . amazonka $ updateFunctionCode pname
                        & uS3Bucket ?~ bucketName
                        & uS3Key    ?~ s3Key


  )
