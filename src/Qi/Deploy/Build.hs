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

buildConfig :: FilePath -> String -> Build
buildConfig srcDir exeTarget = Build srcDir (SimpleTarget exeTarget) "lambda"

build :: Build -> IO FilePath
build Build{..} = do
  let (SimpleTarget exeTarget) = lambdaTarget
      imageName = "ghc-centos:" ++ exeTarget
  buildDocker imageName
  -- build executable with docker
  exe <- stackInDocker (ImageName imageName) lambdaSrcDirectory lambdaTarget
  -- pack executable with js shim in .zip file
  packLambda exe finalProgName
  return "lambda.zip"
    where
      buildDocker :: String -> IO ()
      buildDocker imageName = callProcess "docker" ["build", "-t", imageName, "ghc-centos" ]

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
