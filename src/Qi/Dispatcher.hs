{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Dispatcher (
    invokeLambda
  , deployApp
  , createCfStack
  , describeCfStack
  , destroyCfStack
  , go
  , renderCfTemplate
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Lens
import           Control.Monad                 (void, (<=<))
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Aeson                    (ToJSON)
import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Maybe                    (fromJust, listToMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Generics
import           Network.AWS                   (AWS, send)
import           Network.AWS.CloudFormation
import           Prelude                       hiding (FilePath, log)
import           System.Console.ANSI
import           System.Environment.Executable (splitExecutablePath)
import           Turtle                        (FilePath, fromString, liftIO,
                                                sh, toText)

import qualified Qi.Amazonka                   as A
import           Qi.Config.AWS                 (Config, getAll, getPhysicalName,
                                                namePrefix)
import           Qi.Config.AWS.S3              (S3Bucket)
import qualified Qi.Config.CF                  as CF
import           Qi.Dispatcher.Build           (build)
import           Qi.Dispatcher.Lambda          (invokeLambda)
import           Qi.Dispatcher.S3              (clearBuckets, createBucket,
                                                putObject)


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

renderCfTemplate :: Dispatcher ()
renderCfTemplate =
   withConfig $ liftIO . LBS.putStr . CF.render

deployApp :: Dispatcher ()
deployApp =
  withConfig $ \config -> do
    let appName = config^.namePrefix

    content <- liftIO $ do
      (_, execFilename) <- splitExecutablePath -- get the current executable filename
      lambdaPackagePath <- fromString <$> build "." execFilename
      LBS.readFile . T.unpack $ toTextIgnore lambdaPackagePath

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

    runAmazonka $ do
      void . send $ createStack appName
                  & csTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", appName, "/cf.json"]
                  & csCapabilities .~ [CapabilityNamedIAM]

    printPending "waiting on the stack to be created..."
    waitOnStackStatus SSCreateComplete False
    printSuccess "stack was successfully created"


data StackDescription = StackDescription {
    sdStatus  :: Text
  , sdOutputs :: [(Text, Text)]
  } deriving (Generic, Show)
instance ToJSON StackDescription

describeCfStack :: Dispatcher ()
describeCfStack =
  withAppName $ \appName -> do
    output <- runAmazonka $ do
      r <- send $ describeStacks
                    & dStackName ?~ appName
      case listToMaybe $ r^.dsrsStacks of
        Just stack ->
          return $ StackDescription {
              sdStatus = T.pack . show $ stack^.sStackStatus
            , sdOutputs = map (\o -> (fromJust $ o^.oOutputKey, fromJust $ o^.oOutputValue)) $ stack^.sOutputs
            }
        Nothing ->
          error "Error: no stack description was returned"
    liftIO . LBS.putStrLn $ encodePretty output


destroyCfStack
  :: Dispatcher ()
  -> Dispatcher ()
destroyCfStack action =
  withConfig $ \config -> do
    let appName = config^.namePrefix

    printSuccess "destroying the stack..."

    runAmazonka $ do
      clearBuckets $ map (getPhysicalName config) (getAll config :: [S3Bucket])
      void . send $ deleteStack appName
                  & dsRetainResources .~ []

    action
    printPending "waiting on the stack to be destroyed..."
    waitOnStackStatus SSDeleteComplete True
    printSuccess "stack was successfully destroyed"


go :: Dispatcher ()
go = do
    destroyCfStack $ do
      printSuccess "deploying the app..."
      deployApp
    createCfStack
    printSuccess "all done!"



waitOnStackStatus
  :: StackStatus
  -> Bool
  -> Dispatcher ()
waitOnStackStatus status absentOk =
  withConfig wait
  where
    wait config = do
      let appName = config^.namePrefix
      stacks <- filter ((==appName) . fst)
                . map (\ss -> (ss^.ssStackName, ss^.ssStackStatus))
                . (^.lsrsStackSummaries)
                <$> runAmazonka (send listStacks)
      case listToMaybe stacks of
        Just (_, s) | s == status -> return ()
        Just (_, _) -> loop -- wait for the stack state to change
        Nothing -> if absentOk -- no mention of the stack in the log
                    then return () -- it's fine, don't wait any longer
                    else loop -- keep waiting for the stack to appear in the log
        where
          loop = waitASecond >> wait config
          waitASecond = liftIO $ threadDelay 10000000


printSuccess = printVivid Green
printPending = printVivid Yellow
printVivid color t = liftIO $ do
  setSGR [SetColor Foreground Vivid color]
  putStr t
  setSGR [Reset]
  putStr "\n"
