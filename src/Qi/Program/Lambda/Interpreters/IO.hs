{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interpreters.IO (run) where

import           Control.Lens                 hiding (view)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Operational
import           Control.Monad.Trans.AWS      (AWST, runAWST, send)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.ByteString.Lazy         (ByteString)
import qualified Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkLbs)
import           Data.Default                 (def)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Network.AWS                  hiding (send)
import qualified Network.AWS.S3               as A
import           System.IO                    (stdout)

import           Qi.Amazonka                  (currentRegion)
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface  (LambdaInstruction (..),
                                               LambdaProgram)

type QiAWS = AWST (ReaderT Config (ResourceT IO))

run
  :: LambdaProgram ()
  -> Config
  -> IO ()
run program config = do
  logger <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion
  runResourceT . (`runReaderT` config) . runAWST env $ interpret program

  where
    interpret
      :: LambdaProgram ()
      -> QiAWS ()
    interpret program = do
      case view program of
        (GetS3ObjectContent s3Obj) :>>= is -> do
          interpret . is =<< getS3ObjectContent s3Obj

        (PutS3ObjectContent s3Obj content) :>>= is -> do
          interpret . is =<< putS3ObjectContent s3Obj content

        Return _ ->
          return def

      where
        getS3ObjectContent
          :: S3Object
          -> QiAWS ByteString
        getS3ObjectContent s3Obj@S3Object{s3oBucketId, s3oKey = S3Key objKey} = do
          {- putStrLn $ "GetS3ObjectContent: " ++ show s3Obj -}
          config <- lift ask
          let bucketName = (getS3BucketById s3oBucketId config) ^. s3bName
          say "Reading s3 object content..." ""
          r <- send . A.getObject (A.BucketName $ bucketName `namePrefixWith` config) $ A.ObjectKey objKey
          sinkBody (r ^. A.gorsBody) sinkLbs

-- TODO: add a streaming version that takes a sink and streams the rsBody into it
-- sinkBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a


        putS3ObjectContent
          :: S3Object
          -> ByteString
          -> QiAWS ()
        putS3ObjectContent s3Obj@S3Object{s3oBucketId, s3oKey = S3Key objKey} content = do
          {- putStrLn $ "PutS3ObjectContent: " ++ show s3Obj -}
          config <- lift ask
          let bucketName = (getS3BucketById s3oBucketId config) ^. s3bName
          say "Writing s3 object content..." ""
          r <- send . A.putObject (A.BucketName $ bucketName `namePrefixWith` config) (A.ObjectKey objKey) $ toBody content
          return ()

        say :: Show a => Text -> a -> QiAWS ()
        say msg = liftIO . T.putStrLn . mappend msg . T.pack . show
