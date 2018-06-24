{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Qi.CLI.Dispatcher.Build(build) where

import           Protolude
import           Qi.Util
import           System.Build       (BuildArgs (SimpleTarget), stackInDocker)
import           System.Directory
import           System.Docker
import           System.Posix.Files
import           System.Posix.Types
import           Text.Heredoc       (there)


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

