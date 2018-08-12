{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.S3.Lang where

import           Control.Monad.Freer
import qualified Data.ByteString.Lazy as LBS
import           Network.AWS.S3.Types (ETag)
import           Protolude
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Qi.Program.Gen.Lang


type S3LambdaProgram effs = S3Event -> Eff effs LBS.ByteString

data S3Eff r where
  GetS3ObjectContent
    :: S3Object
    -> S3Eff (Either Text LBS.ByteString)
{-
  MultipartS3Upload
    :: S3Object
    -> (S3Object -> Text -> LambdaProgram [(Int, ETag)])
    -> S3Eff ()

  UploadS3Chunk
    :: S3Object -- sink
    -> Text -- uploadId
    -> (Int, S3Object) -- source chunk
    -> S3Eff (Maybe (Int, ETag))
-}

  PutS3ObjectContent
    :: S3Object
    -> LBS.ByteString
    -> S3Eff ()
{-
  ListS3Objects
    :: Monoid a
    => S3BucketId
    -> (a -> [S3Object] -> LambdaProgram a)
    -> S3Eff a
-}
  DeleteS3Object
    :: S3Object
    -> S3Eff ()

  DeleteS3Objects
    :: [S3Object]
    -> S3Eff ()


getS3ObjectContent
  :: (Member S3Eff effs)
  => S3Object
  -> Eff effs (Either Text LBS.ByteString)
getS3ObjectContent = send . GetS3ObjectContent

{-
multipartS3Upload
  :: (Member S3Eff effs)
  => S3Object
  -> (S3Object -> Text -> LambdaProgram [(Int, ETag)])
  -> Eff effs ()
multipartS3Upload = send .: MultipartS3Upload

uploadS3Chunk
  :: (Member S3Eff effs)
  => S3Object -- sink
  -> Text -- uploadId
  -> (Int, S3Object) -- source chunk
  -> Eff effs (Maybe (Int, ETag))
uploadS3Chunk = send .:: UploadS3Chunk
-}

putS3ObjectContent
  :: (Member S3Eff effs)
  => S3Object
  -> LBS.ByteString
  -> Eff effs ()
putS3ObjectContent = send .: PutS3ObjectContent

{-
listS3Objects
  :: (Monoid a, Member S3Eff effs)
  => S3BucketId
  -> (a -> [S3Object] -> LambdaProgram a)
  -> Eff effs a
listS3Objects = send .: ListS3Objects
-}

deleteS3Object
  :: (Member S3Eff effs)
  => S3Object
  -> Eff effs ()
deleteS3Object = send . DeleteS3Object

deleteS3Objects
  :: (Member S3Eff effs)
  => [S3Object]
  -> Eff effs ()
deleteS3Objects = send . DeleteS3Objects


