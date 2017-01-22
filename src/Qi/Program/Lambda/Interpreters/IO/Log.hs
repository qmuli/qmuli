{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Program.Lambda.Interpreters.IO.Log (
    LogQueue
  , queueLogEntry
  , signalLogEnd
  , forkIOSync
  , cloudWatchLoggerWorker
  ) where

import           Control.Applicative        ((<*>))
import           Control.Concurrent         hiding (yield)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM     (STM, TBQueue, atomically,
                                             isEmptyTBQueue, readTBQueue,
                                             writeTBQueue)
import           Control.Exception.Base     (SomeException)
import           Control.Lens               hiding (view)
import           Control.Monad              (void, (<=<))
import           Control.Monad.Extra        (ifM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Trans.AWS    (AWST, runAWST, send)
import           Control.Monad.Trans.Class  (lift)
import           Data.Binary.Builder        (fromLazyByteString,
                                             toLazyByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Conduit               (Conduit, Sink, awaitForever,
                                             transPipe, unwrapResumable, yield,
                                             ($$), (=$=))
import qualified Data.Conduit.List          as CL
import           Data.Default               (def)
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           GHC.Exts                   (fromList)
import           Network.AWS                hiding (Request, Response, send)
import           Network.AWS.CloudWatchLogs
import           Network.AWS.Data.Body      (RsBody (..), fuseStream)
import           System.IO                  (hPutStrLn, stderr)

import           Qi.Amazonka                (currentRegion)
import           Qi.Config.AWS
import           Qi.Util                    (time)


type LogQueue = TBQueue (Maybe InputLogEvent)

toInputLogEvent
  :: Text
  -> IO InputLogEvent
toInputLogEvent entry = do
  -- get current UTC epoch time in milliseconds
  ts <- fromIntegral . round . (* 1000) <$> getPOSIXTime
  return $ inputLogEvent ts entry

queueLogEntry
  :: LogQueue
  -> Text
  -> IO ()
queueLogEntry q =
  atomically . writeTBQueue q . Just <=< toInputLogEvent

signalLogEnd
  :: LogQueue
  -> IO ()
signalLogEnd q =
  atomically $ writeTBQueue q Nothing

readAllAvailableTBQueue
  :: TBQueue a
  -> STM [a]
readAllAvailableTBQueue q =
  readWithLimit limit
  where
    limit = 100
    readWithLimit 0 = pure []
    readWithLimit count =
      ifM (isEmptyTBQueue q)
        (pure [])
        $ (:) <$> readTBQueue q <*> readWithLimit (count - 1)


cloudWatchLoggerWorker
  :: Text
  -> Config
  -> LogQueue
  -> IO ()
cloudWatchLoggerWorker lbdName config q = do
  {- logger <- newLogger Debug stdout -}
  {- stdLoggerEnv <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion -}
  noLoggerEnv <- newEnv Discover <&> set envRegion currentRegion

  {- runResourceT . runAWST stdLoggerEnv $ -}
  runResourceT . runAWST noLoggerEnv $
    loop =<< ensureLogStream

  where
    groupName = config^.namePrefix
    streamName = lbdName

    -- TODO: this ensuring takes way too long, about 1s - this is unacceptable for lambdas
    -- that finish in much shorter time
    ensureLogStream = do
      groups <- paginate describeLogGroups $$ CL.foldMap (^.dlgrsLogGroups)
      case listToMaybe $ filter ( (== Just groupName) . (^.lgLogGroupName) ) groups of
        Just group -> do
          streams <- paginate (describeLogStreams groupName) $$ CL.foldMap (^.dlsrsLogStreams)
          case listToMaybe $ filter ( (== Just streamName) . (^.lsLogStreamName) ) streams of
            Just stream ->
              return $ stream^.lsUploadSequenceToken
            Nothing -> do
              clsr <- send $ createLogStream groupName streamName
              return Nothing
        Nothing -> do
          clgr <- send $ createLogGroup groupName
          -- ignore response
          clsr <- send $ createLogStream groupName streamName
          -- ignore response
          return Nothing


    sendIfAny [] nextToken = return nextToken
    sendIfAny msgs nextToken = fmap (^.plersNextSequenceToken)
      . send $ putLogEvents groupName streamName (fromList msgs)
                     & pleSequenceToken .~ nextToken

    loop
      :: Maybe Text
      -> AWS ()
    loop nextToken = do
      -- wait for 50 ms before trying to dequeue messages, so we dont have to wait the full 200ms
      -- in case lambda takes less than that to finish
      liftIO . threadDelay $ 50 * 1000 -- takes microseconds

      maybeMsgs <- liftIO $
        atomically $ readAllAvailableTBQueue q

      let msgs = catMaybes maybeMsgs
      -- is there a terminating Nothing at the end of messages list retrieved from the channel?
      if length msgs /= length maybeMsgs
        then do
          {- endSignal <- liftIO $ toInputLogEvent "Logger exitting..." -}
          {- void $ sendIfAny (msgs ++ [endSignal]) nextToken -}
          void $ sendIfAny msgs nextToken
        else do
          -- CW Logs limits: https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/cloudwatch_limits_cwl.html
          -- wait for 200ms (including the 50ms above) to rate-limit the log sending
          liftIO . threadDelay $ 150 * 1000 -- takes microseconds
          loop =<< sendIfAny msgs nextToken

-- this allows to wait on the forked child thread
forkIOSync
  :: IO ()
  -> IO (MVar ())
forkIOSync io = do
  mvar <- newEmptyMVar
  forkFinally io $ either
                      (\(e :: SomeException) -> do
                        hPutStrLn stderr "Error occured in the CloudWatch Logger:"
                        hPutStrLn stderr $ show e
                        putMVar mvar ()
                      )
                      (putMVar mvar)
  return mvar


