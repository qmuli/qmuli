{-# LANGUAGE RecordWildCards #-}
module Qi.Deploy.Build where

import           System.Build
import           System.Docker
import           System.Process

data Build = Build { lambdaSrcDirectory :: FilePath
                   , lambdaTargetName   :: String
                   }

build :: Build -> IO FilePath
build Build{..} = do
  buildDocker
  -- build executable with docker
  exe <- stackInDocker (ImageName "ghc-centos:qmuli") lambdaSrcDirectory lambdaTargetName
  -- pack executable with js shim in .zip file
  packLambda exe
  return "lambda.zip"
    where
      buildDocker :: IO ()
      buildDocker = callProcess "docker" ["build", "-t", "ghc-centos:qmuli","ghc-centos" ]

      packLambda :: FilePath -> IO ()
      packLambda source = do
        runner <- readFile "js/index.js"
        writeFile "run.js" runner
        callProcess "zip" $ [ "lambda.zip", "run.js" , source ]
