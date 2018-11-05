{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens                         hiding (argument)
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Aeson                           (Value, eitherDecode,
                                                       encode)
import           Data.Aeson.Types                     (parseEither)
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Char                            (isDigit, isLower)
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           Protolude                            hiding (State, getAll,
                                                       runState)
import           Qi.AWS.Logger
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
import qualified Qi.Config.AWS.ApiGw.ApiMethod.Event  as ApiMethodEvent (parse)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CfCustomResource.Types (CfCustomResourceEvent)
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import qualified Qi.Config.AWS.Lambda.Accessors       as Lbd
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event               as S3Event
import qualified Qi.Config.CfTemplate                 as CF
import           Qi.Config.Types                      (ResourceExistence (AlreadyExists))
import           Qi.Options
import qualified Qi.Program.Config.Ipret.State        as Config
import           Qi.Program.Config.Lang               (ConfigEff, s3Bucket)
import qualified Qi.Program.Gen.Lang                  as Gen
import qualified Qi.Program.Lambda.Lang               as Lbd
import qualified Qi.Program.Wiring.IO                 as IO
import           System.IO                            (BufferMode (..),
                                                       hSetBuffering, stderr,
                                                       stdout)


withConfig
  :: Eff '[ConfigEff, State Config] ()
  -> IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  Options{ appName, cmd, awsMode } <- showHelpOnErrorExecParser optionsSpec

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let template = CF.render config
      config =
          snd
        . run
        . runState def{_namePrefix = appName}
        . Config.run
        $ do
          s3Bucket "app" $ def & s3bpExistence .~ AlreadyExists -- always assume existence of the app bucket
          configProgram

      runCli = IO.run "dispatcher" config awsMode mkCliLogger
      runLambda name = IO.run name config awsMode mkLambdaLogger

  case cmd of
    CfRenderTemplate     -> runCli . Gen.say $ toS template
    CfDeploy             ->
      runCli $
            Gen.build
        >>= Gen.readFileLazy
        >>= deployApp template

    CfCreate             -> runCli $ createCfStack template
    CfUpdate             -> runCli $ updateCfStack template
    CfDescribe           -> runCli   describeCfStack
    CfDestroy            -> runCli . destroyCfStack $ pure ()

    CfCycle              ->
      runCli $
            Gen.build
        >>= Gen.readFileLazy
        >>= cycleStack template

    LbdUpdate -> runCli $ updateLambdas
    LbdSendEvent name -> runLambda name $ do
      arg <- Gen.getLine

      -- get lambda id from name
      let id = Lbd.getIdByName config name
          reportBadArgument lbdType err =
            panic $ "Could not parse event: '" <> toS arg <>
              "', for lambda type: '" <> lbdType <> "' error was: '" <> toS err <> "'"

      Gen.putStr =<< case getById config id of

        GenericLambda{ _lbdGenericLambdaProgram } ->
          either  (reportBadArgument "Generic")
                  (map encode . _lbdGenericLambdaProgram)
                  $ eitherDecode (toS arg)

        S3BucketLambda{ _lbdS3BucketLambdaProgram } ->
          either  (reportBadArgument "S3")
                  _lbdS3BucketLambdaProgram
                  $ parseEither (S3Event.parse config) =<< eitherDecode (toS arg)

        CfCustomLambda{ _lbdCfCustomLambdaProgram } ->
          either  (reportBadArgument "CfCustom")
                  _lbdCfCustomLambdaProgram
                  $ eitherDecode (toS arg)

        CwEventLambda{ _lbdCwEventLambdaProgram } ->
          either  (reportBadArgument "CW")
                  _lbdCwEventLambdaProgram
                  $ eitherDecode (toS arg)

    LbdLogs _ -> pass -- lambdaLogs name

  where




{-
    parseLambdaEvent ApiLambda{_lbdApiMethodLambdaProgram} eventJson =
      _lbdApiMethodLambdaProgram <$> (parseEither (ApiMethodEvent.parse config) =<< eitherDecode (toS eventJson))


    parseLambdaEvent DdbStreamLambda{_lbdDdbStreamLambdaProgram} eventJson =
      _lbdDdbStreamLambdaProgram <$> (eitherDecode (toS eventJson) :: Either [Char] DdbStreamEvent)

-}


