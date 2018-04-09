{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Qi.Dispatcher.Build(build) where

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
  -- pack executable with js shim in .zip file
  packLambda exe "lambda"
  return "lambda.zip"
    where
      buildDocker :: Text -> IO ()
      buildDocker imageName = callProcess "docker" ["build", "-t", toS imageName, "ghc-centos" ]

      executableByAll :: FileMode
      executableByAll = foldl unionFileModes nullFileMode [ ownerModes
                                                          , groupReadMode, groupExecuteMode
                                                          , otherReadMode, otherExecuteMode
                                                          ]

      packLambda :: FilePath -> FilePath -> IO ()
      packLambda source target = do
        writeFile "index.js" [there|./js/index.js|]
        copyFile source target
        target `setFileMode` executableByAll
        callProcess "zip" $ [ "lambda.zip", "index.js" , target ]
