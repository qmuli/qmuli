{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.Gen.Ipret.IO  where

import           Control.Lens            hiding (view, (.=))
import           Control.Monad.Freer     hiding (run)
import           Control.Monad.Trans.AWS (AWST, runAWST)
import qualified Control.Monad.Trans.AWS as AWS (send)
import           Data.Aeson              (FromJSON, ToJSON, Value (..), decode,
                                          encode, object, (.=))
import           Data.Conduit.Binary     (sinkLbs)
import qualified Data.Time.Clock         as C
import           Network.AWS             hiding (Request, Response, send)
import           Network.HTTP.Client     (ManagerSettings, Request, Response,
                                          httpLbs, newManager)
import           Protolude               hiding ((<&>))
import           Qi.Amazonka             (currentRegion)
import           Qi.Config.AWS
import           Qi.Program.Config.Lang  (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Servant.Client          (BaseUrl, ClientM, ServantError,
                                          mkClientEnv, runClientM)
import           System.IO               (hFlush, stdout)


run
  :: forall effs a
  .  (Member IO effs, Member ConfigEff effs)
  => (Eff (GenEff ': effs) a -> Eff effs a)
run = interpret (\case

  GetAppName ->
    (^. namePrefix) <$> getConfig

  Http mgrSettings req -> send $
    httpLbs req =<< newManager mgrSettings

  RunServant mgrSettings baseUrl req -> send $ do
    mgr <- newManager mgrSettings
    runClientM req $ mkClientEnv mgr baseUrl


  Amazonka req -> send $ do
    (pass :: IO ()) -- to force the concrete IO monad
    logger  <- newLogger Debug stdout
    env     <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion

    runResourceT . runAWST env $ AWS.send req


  AmazonkaPostBodyExtract req post -> send $ do
    (pass :: IO ()) -- to force the concrete IO monad
    logger  <- newLogger Debug stdout
    env     <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion

    runResourceT . runAWST env $
      map Right . (`sinkBody` sinkLbs) . post =<< AWS.send req

  Say msg -> send $ do
    putStrLn . encode $ object ["message" .= String msg]
    hFlush stdout

  GetCurrentTime -> send C.getCurrentTime

  Sleep us -> send $ threadDelay us

  )
