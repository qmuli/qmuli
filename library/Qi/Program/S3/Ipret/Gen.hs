{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Qi.Program.S3.Ipret.Gen (run) where

import           Control.Exception.Lens (handling)
import           Control.Lens           (Getting, (.~), (?~), (^.))
import           Control.Monad.Freer    hiding (run)
import qualified Data.Map.Strict        as Map
import           Network.AWS            hiding (Request, Response, send)
import           Network.AWS.Data.Body  (RsBody (..))
import           Network.AWS.Data.Text  (ToText (..))
import           Network.AWS.S3
import           Protolude              hiding ((<&>))
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Program.Config.Lang (ConfigEff, getConfig, s3Bucket)
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Internal (ListToken (ListToken))
import           Qi.Program.S3.Lang     (S3Eff (..))


run
  :: forall effs a
  .  (Member GenEff effs, Member ConfigEff effs)
  => (Eff (S3Eff ': effs) a -> Eff effs a)
run = interpret (\case

  CreateBucket name -> do
    id      <- s3Bucket name
    config  <- getConfig
    let bucketName = BucketName . getPhysicalName config $ getById config id
    amazonka $ createBucket bucketName
    pure id

  GetContent S3Object{ _s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey) } ->
    action =<< getConfig
    -- TODO: handle errors
    -- handling _KeyNotFound handler action

    where
      action config =
        amazonkaPostBodyExtract
          (getObject bucketName objKey)
          (^. gorsBody)

        where
          bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId



{-
        handler _ = pure $ Left "KeyNotFound"

        _KeyNotFound :: AsError a => Getting (First ServiceError) a ServiceError
        _KeyNotFound = _ServiceError . hasStatus 404 -- . hasCode "InvalidKeyPair.Duplicate"
-}



  PutContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} payload -> do
    config <- getConfig
    let bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId
    void $ amazonka $ putObject bucketName objKey (toBody payload) & poACL ?~ OPublicReadWrite


  ListObjects id maybeToken -> do
    config <- getConfig
    let bucketName = getPhysicalName config $ getById config id
    r <- amazonka $ case maybeToken of
      Nothing -> -- first pagination call
        listObjectsV2 (BucketName bucketName)
      Just (ListToken token) ->
        listObjectsV2 (BucketName bucketName) & lovContinuationToken ?~ token
    let objs = (\o -> S3Object id $ S3Key . toText $ o ^. oKey) <$> (r ^. lovrsContents)

    pure $ (objs, ListToken <$> r ^. lovrsNextContinuationToken)



  DeleteObject s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} -> do
    config <- getConfig
    let bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId
    void $ amazonka $ deleteObject bucketName objKey


  DeleteObjects s3objs -> do
    config <- getConfig
    let dict = Map.toList . Map.fromListWith (<>) $ toPair <$> s3objs

        toPair :: S3Object -> (BucketName, [ObjectIdentifier])
        toPair s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} =
          ( BucketName . getPhysicalName config $ getById config _s3oBucketId
          , [objectIdentifier objKey]
          )

    for_ dict $ \(bucketName, objIds) ->
      amazonka $ deleteObjects bucketName $ delete' & dObjects .~ objIds

  )

