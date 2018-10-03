{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}


module Qi.Program.Gen.Ipret.IO  where

import           Control.Lens                  hiding (view, (.=))
import           Control.Monad.Freer           hiding (run)
import           Control.Monad.Trans.AWS       (AWST, runAWST)
import qualified Control.Monad.Trans.AWS       as AWS (send)
import           Data.Aeson                    (FromJSON, ToJSON, Value (..),
                                                decode, encode, object, (.=))
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Conduit.Binary           (sinkLbs)
import qualified Data.Time.Clock               as C
import           Network.AWS                   hiding (Request, Response, send)
import           Network.HTTP.Client           (ManagerSettings, Request,
                                                Response, httpLbs, newManager)
import           Protolude                     hiding ((<&>))
import           Protolude
import           Qi.Amazonka                   (currentRegion)
import           Qi.Config.AWS
import           Qi.Program.Config.Lang        (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang           (GenEff (..))
import           Qi.Util
import           Servant.Client                (BaseUrl, ClientM, ServantError,
                                                mkClientEnv, runClientM)
import           System.Build                  (BuildArgs (SimpleTarget),
                                                stackInDocker)
import           System.Directory
import           System.Docker
import           System.Environment.Executable (splitExecutablePath)
import           System.IO                     (hFlush, stdout)
import           System.Posix.Files
import           System.Posix.Types
import           Text.Heredoc                  (there)


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

  Build -> send $ do
    (_, execFilename) <- splitExecutablePath -- get the current executable filename
    toS <$> build "." (toS execFilename)

  ReadFileLazy path ->
    send . LBS.readFile $ toS path

  GetLine -> send $ BS.getLine

  PutStr content -> send $ LBS.putStr content

  )


build
  :: FilePath
  -> Text
  -> IO FilePath
build srcDir exeTarget = do
  let imageName = "ghc-centos:" <> exeTarget

  buildDocker imageName
  -- build executable with docker
  printPending $ "building with srcDir: '" <> toS srcDir <> "' and exeTarget: '" <> exeTarget <> "' ..."
  exe <- stackInDocker (ImageName $ toS imageName) srcDir (SimpleTarget $ toS exeTarget)

  let buildDir = srcDir <> "/.build"
  -- ensure hidden build dir exists
  createDirectoryIfMissing True buildDir

  -- move and rename the exe to the .build dir
  let lambdaPath = buildDir <> "/lambda"
  renameFile exe lambdaPath
  setFileMode lambdaPath executableByAll

  -- pack executable with js shim in .zip file
  let archivePath = buildDir <> "/lambda.zip"
      jsShimPath  = buildDir <> "/index.js"
  writeFile jsShimPath [there|./js/index.js|]
  callProcess "zip" $ [ "-j", archivePath, jsShimPath, lambdaPath ]

  pure archivePath

    where
      buildDocker :: Text -> IO ()
      buildDocker imageName = callProcess "docker" ["build", "-t", toS imageName, "ghc-centos" ]

      executableByAll :: FileMode
      executableByAll = foldl unionFileModes nullFileMode [ ownerModes
                                                          , groupReadMode, groupExecuteMode
                                                          , otherReadMode, otherExecuteMode
                                                          ]



