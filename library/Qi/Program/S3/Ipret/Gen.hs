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
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Internal (ListToken (ListToken))
import           Qi.Program.S3.Lang     (ListToken, S3Eff (..))


run
  :: forall effs a
  .  (Member GenEff effs)
  => Config
  -> (Eff (S3Eff ': effs) a -> Eff effs a)
run config = interpret (\case

  CreateBucket name ->
    void $ amazonka $ createBucket (BucketName name)


  GetContent S3Object{ _s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey) } ->
    action
    -- TODO: handle errors
    -- handling _KeyNotFound handler action

    where
      bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId

      action =
        amazonkaPostBodyExtract
          (getObject bucketName objKey)
          (^. gorsBody)


{-
        handler _ = pure $ Left "KeyNotFound"

        _KeyNotFound :: AsError a => Getting (First ServiceError) a ServiceError
        _KeyNotFound = _ServiceError . hasStatus 404 -- . hasCode "InvalidKeyPair.Duplicate"
-}



  PutContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} payload ->
    void $ amazonka $ putObject bucketName objKey (toBody payload) & poACL ?~ OPublicReadWrite

    where
      bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId


  ListObjects id maybeToken -> do
    r <- amazonka $ case maybeToken of
      Nothing -> -- first pagination call
        listObjectsV2 (BucketName bucketName)
      Just (ListToken token) ->
        listObjectsV2 (BucketName bucketName) & lovContinuationToken ?~ token
    let objs = (\o -> S3Object id $ S3Key . toText $ o ^. oKey) <$> (r ^. lovrsContents)

    pure $ (objs, ListToken <$> r ^. lovrsNextContinuationToken)
    where
      bucketName = getPhysicalName config $ getById config id



  DeleteObject s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} ->
    void $ amazonka $ deleteObject bucketName objKey

    where
      bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId


  DeleteObjects s3objs ->
    for_ dict $ \(bucketName, objIds) ->
      amazonka $ deleteObjects bucketName $ delete' & dObjects .~ objIds

    where
      dict = Map.toList . Map.fromListWith (<>) $ toPair <$> s3objs

      toPair :: S3Object -> (BucketName, [ObjectIdentifier])
      toPair s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} =
        ( BucketName . getPhysicalName config $ getById config _s3oBucketId
        , [objectIdentifier objKey]
        )


  )

