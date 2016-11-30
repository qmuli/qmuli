{-# LANGUAGE RecordWildCards #-}
module Qi.Deploy.Build(build, buildConfig, Build) where

import           System.Build
import           System.Docker
import           System.Process

data Build = Build { lambdaSrcDirectory :: FilePath
                   , lambdaTarget       :: BuildTarget
                   }

buildConfig :: FilePath -> String -> Build
buildConfig srcDir exeTarget = Build srcDir (FullTarget "qmuli" exeTarget)

build :: Build -> IO FilePath
build Build{..} = do
  buildDocker
  -- build executable with docker
  exe <- stackInDocker (ImageName "ghc-centos:qmuli") lambdaSrcDirectory lambdaTarget
  -- pack executable with js shim in .zip file
  packLambda exe
  return "lambda.zip"
    where
      buildDocker :: IO ()
      buildDocker = callProcess "docker" ["build", "-t", "ghc-centos:qmuli","ghc-centos" ]

      packLambda :: FilePath -> IO ()
      packLambda source = do
        runner <- readFile "js/index.js"
        writeFile "index.js" runner
        callProcess "zip" $ [ "lambda.zip", "index.js" , source ]
