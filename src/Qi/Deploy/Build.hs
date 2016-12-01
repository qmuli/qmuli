{-# LANGUAGE RecordWildCards #-}
module Qi.Deploy.Build(build, buildConfig, Build) where

import           System.Build
import           System.Directory
import           System.Docker
import           System.Posix.Files
import           System.Posix.Types
import           System.Process

data Build = Build { lambdaSrcDirectory :: FilePath
                   , lambdaTarget       :: BuildTarget
                   , finalProgName      :: FilePath
                   }

buildConfig :: FilePath -> String -> String -> Build
buildConfig srcDir exeTarget finalName = Build srcDir (FullTarget "qmuli" exeTarget) finalName

build :: Build -> IO FilePath
build Build{..} = do
  buildDocker
  -- build executable with docker
  exe <- stackInDocker (ImageName "ghc-centos:qmuli") lambdaSrcDirectory lambdaTarget
  -- pack executable with js shim in .zip file
  packLambda exe finalProgName
  return "lambda.zip"
    where
      buildDocker :: IO ()
      buildDocker = callProcess "docker" ["build", "-t", "ghc-centos:qmuli","ghc-centos" ]

      executableByAll :: FileMode
      executableByAll = foldl unionFileModes nullFileMode [ ownerModes
                                                          , groupReadMode, groupExecuteMode
                                                          , otherReadMode, otherExecuteMode
                                                          ]

      packLambda :: FilePath -> FilePath -> IO ()
      packLambda source target = do
        runner <- readFile "js/index.js"
        writeFile "index.js" runner
        copyFile source target
        target `setFileMode` executableByAll
        callProcess "zip" $ [ "lambda.zip", "index.js" , target ]
