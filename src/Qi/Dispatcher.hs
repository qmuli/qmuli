{-# LANGUAGE OverloadedStrings #-}

module Qi.Dispatcher (
    invokeLambda
  , updateLambdas
  , deployApp
  , createCfStack
  , updateCfStack
  , describeCfStack
  , destroyCfStack
  , cycleStack
  , renderCfTemplate
  ) where

import           Control.Lens
import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T
import           Network.AWS                   (AWS, send)
import           Protolude                     hiding (FilePath, getAll)
import           System.Environment.Executable (splitExecutablePath)
import           Turtle                        (FilePath, fromString, toText)

import qualified Qi.Amazonka                   as A
import           Qi.Config.AWS                 (Config, getAll, getPhysicalName,
                                                namePrefix)
import           Qi.Config.AWS.Lambda          (Lambda)
import           Qi.Config.AWS.S3              (S3Bucket)
import qualified Qi.Config.CF                  as CF
import           Qi.Dispatcher.Build           (build)
import           Qi.Dispatcher.CF              (createStack, deleteStack,
                                                describeStack, updateStack,
                                                waitOnStackCreated,
                                                waitOnStackDeleted,
                                                waitOnStackUpdated)
import qualified Qi.Dispatcher.Lambda          as Lambda (invoke, update)
import           Qi.Dispatcher.S3              (clearBuckets, createBucket,
                                                putObject)
import           Qi.Util                       (printPending, printSuccess)


type Dispatcher = ReaderT Config IO

withConfig
  :: (Config -> Dispatcher ())
  -> Dispatcher ()
withConfig action = action =<< ask

withAppName
  :: (Text -> Dispatcher ())
  -> Dispatcher ()
withAppName action = withConfig $ action . (^.namePrefix)

runAmazonka
  :: AWS a
  -> Dispatcher a
runAmazonka = liftIO . A.runAmazonka



invokeLambda = Lambda.invoke

updateLambdas :: Dispatcher ()
updateLambdas = do
  withConfig $ \config -> do
    let appName = config^.namePrefix
    printSuccess "updating the lambdas..."
    runAmazonka . Lambda.update appName $ map (getPhysicalName config) (getAll config :: [Lambda])


renderCfTemplate :: Dispatcher ()
renderCfTemplate =
   withConfig $ liftIO . LBS.putStr . CF.render

deployApp :: Dispatcher ()
deployApp =
  withConfig $ \config -> do
    let appName = config^.namePrefix

    printSuccess "deploying the app..."
    content <- liftIO $ do
      (_, execFilename) <- splitExecutablePath -- get the current executable filename
      lambdaPackagePath <- fromString <$> build "." (toS execFilename)
      LBS.readFile . toS $ toTextIgnore lambdaPackagePath

    runAmazonka $ do
      createBucket appName
      putObject appName "cf.json" $ CF.render config
      putObject appName "lambda.zip" content

  where
    toTextIgnore :: FilePath -> T.Text
    toTextIgnore x = case toText x of
      Right s -> s
      Left _  -> ""


createCfStack :: Dispatcher ()
createCfStack =
  withAppName $ \appName -> do
    printSuccess "creating the stack..."
    runAmazonka $ createStack appName
    printPending "waiting on the stack to be created..."
    liftIO $ waitOnStackCreated appName
    printSuccess "stack was successfully created"


updateCfStack :: Dispatcher ()
updateCfStack =
  withAppName $ \appName -> do
    printSuccess "updating the stack..."
    runAmazonka $ updateStack appName
    printPending "waiting on the stack to be updated..."
    liftIO $ waitOnStackUpdated appName
    -- TODO: make lambda updating concurrent with the above stack update?
    updateLambdas
    printSuccess "stack was successfully updated"

describeCfStack :: Dispatcher ()
describeCfStack =
  withAppName $ liftIO . LBS.putStrLn . encodePretty
                  <=< runAmazonka . describeStack


destroyCfStack
  :: Dispatcher ()
  -> Dispatcher ()
destroyCfStack action =
  withConfig $ \config -> do
    let appName = config^.namePrefix

    printSuccess "destroying the stack..."
    runAmazonka $ do
      clearBuckets $ map (getPhysicalName config) (getAll config :: [S3Bucket])
      deleteStack appName

    action

    printPending "waiting on the stack to be destroyed..."
    liftIO $ waitOnStackDeleted appName
    printSuccess "stack was successfully destroyed"


cycleStack :: Dispatcher ()
cycleStack = do
    destroyCfStack $ do
      deployApp
    createCfStack
    printSuccess "all done!"




