{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Program.Lambda.Interpreters.IO.Log (
    LogChan
  , queueLogEntry
  , signalLogEnd
  , forkIOSync
  , cloudWatchLoggerWorker
  ) where

import           Control.Applicative        ((<*>))
import           Control.Concurrent         hiding (yield)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
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

import           Qi.Amazonka                (currentRegion)
import           Qi.Config.AWS


type LogChan = TChan (Maybe InputLogEvent)

toInputLogEvent :: Text -> IO InputLogEvent
toInputLogEvent entry = do
  -- get current UTC epoch time in milliseconds
  ts <- fromIntegral . round . (* 1000) <$> getPOSIXTime
  return $ inputLogEvent ts entry

queueLogEntry
  :: LogChan
  -> Text
  -> IO ()
queueLogEntry chan =
  atomically . writeTChan chan . Just <=< toInputLogEvent

signalLogEnd
  :: LogChan
  -> IO ()
signalLogEnd chan =
  atomically $ writeTChan chan Nothing

readAllAvailableTChan
  :: TChan a
  -> STM [a]
readAllAvailableTChan chan =
  ifM (isEmptyTChan chan)
    (pure [])
    $ (:) <$> readTChan chan <*> readAllAvailableTChan chan

cloudWatchLoggerWorker
  :: Text
  -> Config
  -> LogChan
  -> IO ()
cloudWatchLoggerWorker lbdName config chan = do
  {- logger <- newLogger Debug stdout -}
  {- stdLoggerEnv <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion -}
  noLoggerEnv <- newEnv Discover <&> set envRegion currentRegion

  {- runResourceT . runAWST stdLoggerEnv $ -}
  runResourceT . runAWST noLoggerEnv $
    loop =<< ensureLogStream

  where
    groupName = config^.namePrefix
    streamName = lbdName

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


    sendIfAny [] nextToken =
      return nextToken
    sendIfAny msgs nextToken = fmap (^.plersNextSequenceToken)
      . send $ putLogEvents groupName streamName (fromList msgs)
                     & pleSequenceToken .~ nextToken

    loop
      :: Maybe Text
      -> AWS ()
    loop nextToken = do
      maybeMsgs <- liftIO $ do
        -- CW Logs limits: https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/cloudwatch_limits_cwl.html
        -- wait for 250ms to rate-limit the log sending
        threadDelay $ 250 * 1000 -- takes microseconds
        atomically $ readAllAvailableTChan chan

      let msgs = catMaybes maybeMsgs
      -- is there a terminating Nothing at the end of messages list retrieved from the channel?
      if length msgs /= length maybeMsgs
        then do
          endSignal <- liftIO $ toInputLogEvent "Logger exitting..."
          void $ sendIfAny (msgs ++ [endSignal]) nextToken
        else
          loop =<< sendIfAny msgs nextToken

-- this allows to wait on the forked child thread
forkIOSync
  :: IO ()
  -> IO (MVar ())
forkIOSync io = do
  mvar <- newEmptyMVar
  forkFinally io $ either
                      (\(e :: SomeException) -> error $ show e)
                      (putMVar mvar)
  return mvar


