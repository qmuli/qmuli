{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Qi.Deploy.Build(build) where

import           System.Build       (stackInDocker, BuildTarget(SimpleTarget))
import           System.Directory
import           System.Docker
import           System.Posix.Files
import           System.Posix.Types
import           System.Process
import           Text.Heredoc       (there)


build :: FilePath -> String -> IO FilePath
build srcDir exeTarget = do
  let imageName = "ghc-centos:" ++ exeTarget

  buildDocker imageName
  -- build executable with docker
  exe <- stackInDocker (ImageName imageName) srcDir (SimpleTarget exeTarget)
  -- pack executable with js shim in .zip file
  packLambda exe "lambda"
  return "lambda.zip"
    where
      buildDocker :: String -> IO ()
      buildDocker imageName = callProcess "docker" ["build", "-t", imageName, "ghc-centos" ]

      executableByAll :: FileMode
      executableByAll = foldl unionFileModes nullFileMode [ ownerModes
                                                          , groupReadMode, groupExecuteMode
                                                          , otherReadMode, otherExecuteMode
                                                          ]

      deployDir = ".deploy"

      packLambda :: FilePath -> FilePath -> IO ()
      packLambda source target = do
        writeFile "index.js" [there|./js/index.js|]
        copyFile source target
        target `setFileMode` executableByAll
        callProcess "zip" $ [ "lambda.zip", "index.js" , target ]
