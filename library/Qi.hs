{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens                   hiding (argument)
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Aeson                     (eitherDecode, encode)
import           Data.Aeson.Types               (parseEither)
import           Data.Default                   (def)
import           Data.Text                      (splitOn)
import           Protolude                      hiding (State, getAll, runState)
import           Qi.AWS.Logger
import           Qi.AWS.Runtime
import           Qi.AWS.Types
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
{- import           Qi.Config.AWS.CW -}
{- import           Qi.Config.AWS.DDB -}
import           Qi.Config.AWS.Lambda           hiding (lbdName)
import qualified Qi.Config.AWS.Lambda.Accessors as Lbd
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event         as S3Event
import qualified Qi.Config.CfTemplate           as CF
import           Qi.Config.Types                (ResourceExistence (AlreadyExists))
import           Qi.Options
import qualified Qi.Program.Config.Ipret.State  as Config
import           Qi.Program.Config.Lang         (ConfigEff, s3Bucket)
import qualified Qi.Program.Gen.Lang            as Gen
import qualified Qi.Program.Wiring.IO           as IO
import           System.Environment             (lookupEnv)
import           System.IO                      (BufferMode (..), hSetBuffering,
                                                 stderr, stdout)


withConfig
  :: Eff '[ConfigEff, State Config] ()
  -> IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  opts <- showHelpOnErrorExecParser optionsSpec

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let mkConfig appName =
          snd
        . run
        . runState def{_namePrefix = appName}
        . Config.run
        $ do
          s3Bucket "app" $ def & s3bpExistence .~ AlreadyExists -- always assume existence of the app bucket
          configProgram

  case opts of
    LbdDispatch -> do
      let splitNames compositeName = case splitOn "_" compositeName of
                        (h:t) -> (h, mconcat t)
                        _ -> panic "could not split the app and lbd names from the composite name"

      (appName, lbdName) <- maybe
                (panic "AWS_LAMBDA_FUNCTION_NAME not found in ENV")
                (splitNames . toS)
              <$> lookupEnv "AWS_LAMBDA_FUNCTION_NAME"

      let config = mkConfig appName
          runLambda = IO.run config RealDeal mkLambdaLogger
      either panic (loop config lbdName runLambda) =<< getEndpoint


    Management ManagementOptions{ appName, cmd, awsMode } -> do
      let config = mkConfig appName
          runCli = IO.run config awsMode mkCliLogger
          template = CF.render config

      runCli $ case cmd of
        CfRenderTemplate     -> Gen.say $ toS template
        CfDeploy             ->
                Gen.build
            >>= Gen.readFileLazy
            >>= deployApp template

        CfCreate             -> createCfStack template
        CfUpdate             -> updateCfStack template
        CfDescribe           -> describeCfStack
        CfDestroy            -> destroyCfStack $ pure ()

        CfCycle              ->
                Gen.build
            >>= Gen.readFileLazy
            >>= cycleStack template

        LbdUpdate -> updateLambdas

        LbdLogs name -> printLogs name

  where

    {- loop -}
      {- :: Config -}
      {- -> Text -}
      {- -> (Text, Int) -}
      {- -> Text -}
      {- -> IO () -}
    loop config lbdName runLambda endpoint = loop'
      where
        loop' = do
          req' <- getWithRetries 3 endpoint
          case req' of
            SuccessResponse req -> do
              resp <- runLambda $ lbdHandler config lbdName $ payload req
              respond endpoint (requestId req) $ SuccessHandlerResponse (toS resp) (Just "application/json")
              loop'

            ErrorResponse code ->
              case code of
                ErrorCode (-1) ->
                  panic ("Failed to send HTTP request to retrieve next task." :: Text)
                _ -> do
                  print ("HTTP request was not successful. HTTP response code: " <>
                     show code <>
                     ". Retrying.." :: Text)
                  loop'


    lbdHandler config name req =
          let id = Lbd.getIdByName config name
              reportBadArgument lbdType err =
                panic $ "Could not parse event: '" <> toS req <>
                  "', for lambda type: '" <> lbdType <> "' error was: '" <> toS err <> "'"
          in
          case getById config id of

            GenericLambda{ _lbdGenericLambdaProgram } ->
              either  (reportBadArgument "Generic")
                      (map encode . _lbdGenericLambdaProgram)
                      $ eitherDecode (toS req)

            S3BucketLambda{ _lbdS3BucketLambdaProgram } ->
              either  (reportBadArgument "S3")
                      _lbdS3BucketLambdaProgram
                      $ parseEither (S3Event.parse config) =<< eitherDecode (toS req)

            CfCustomLambda{ _lbdCfCustomLambdaProgram } ->
              either  (reportBadArgument "CfCustom")
                      _lbdCfCustomLambdaProgram
                      $ eitherDecode (toS req)

            CwEventLambda{ _lbdCwEventLambdaProgram } ->
              either  (reportBadArgument "CW")
                      _lbdCwEventLambdaProgram
                      $ eitherDecode (toS req)






{-
    parseLambdaEvent ApiLambda{_lbdApiMethodLambdaProgram} eventJson =
      _lbdApiMethodLambdaProgram <$> (parseEither (ApiMethodEvent.parse config) =<< eitherDecode (toS eventJson))


    parseLambdaEvent DdbStreamLambda{_lbdDdbStreamLambdaProgram} eventJson =
      _lbdDdbStreamLambdaProgram <$> (eitherDecode (toS eventJson) :: Either [Char] DdbStreamEvent)

-}


