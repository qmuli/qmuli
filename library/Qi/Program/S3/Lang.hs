{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.S3.Lang (
    S3Eff (..)
  , ListToken
  , S3LambdaProgram
  , getContent
  , putContent
  , listObjects
  , deleteObject
  , deleteObjects
  ) where

import           Control.Monad.Freer
import qualified Data.ByteString.Lazy   as LBS
import           Network.AWS.S3.Types   (ETag)
import           Protolude
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Internal (ListToken)


type S3LambdaProgram effs = S3Event -> Eff effs LBS.ByteString

data S3Eff r where


  GetContent
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

  PutContent
    :: S3Object
    -> LBS.ByteString
    -> S3Eff ()

  ListObjects
    :: S3BucketId
    -> Maybe ListToken
    -> S3Eff ([S3Object], Maybe ListToken)

  DeleteObject
    :: S3Object
    -> S3Eff ()

  DeleteObjects
    :: [S3Object]
    -> S3Eff ()


getContent
  :: (Member S3Eff effs)
  => S3Object
  -> Eff effs (Either Text LBS.ByteString)
getContent = send . GetContent

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

putContent
  :: (Member S3Eff effs)
  => S3Object
  -> LBS.ByteString
  -> Eff effs ()
putContent = send .: PutContent


listObjects
  :: (Member S3Eff effs)
  => S3BucketId
  -> Maybe ListToken
  -> Eff effs ([S3Object], Maybe ListToken)
listObjects = send .: ListObjects


deleteObject
  :: (Member S3Eff effs)
  => S3Object
  -> Eff effs ()
deleteObject = send . DeleteObject

deleteObjects
  :: (Member S3Eff effs)
  => [S3Object]
  -> Eff effs ()
deleteObjects = send . DeleteObjects


