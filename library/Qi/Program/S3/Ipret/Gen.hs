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

module Qi.Program.S3.Ipret.Gen  where

import           Control.Exception.Lens   (handling)
import           Control.Lens             (Getting, (.~), (?~), (^.))
import           Control.Monad.Freer
import qualified Data.Map.Strict          as Map
import           Network.AWS              hiding (Request, Response, send)
import           Network.AWS.Data.Body    (RsBody (..))
import           Network.AWS.S3
import           Network.AWS.S3.GetObject (getObject)
import           Network.AWS.S3.PutObject
import           Protolude                hiding ((<&>))
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang


run
  :: forall effs a
  .  (Member GenEff effs)
  => Config
  -> (Eff (S3Eff ': effs) a -> Eff effs a)

run config = interpret (\case

  GetS3ObjectContent S3Object{ _s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey) } ->
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



  PutS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} payload ->
    void $ amazonka $ putObject bucketName objKey (toBody payload) & poACL ?~ OPublicReadWrite

    where
      bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId



  DeleteS3Object s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} ->
    void $ amazonka $ deleteObject bucketName objKey

    where
      bucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId




  DeleteS3Objects s3objs ->
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

