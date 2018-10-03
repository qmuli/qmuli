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
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
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
import           Qi.Options
import qualified Qi.Program.Config.Ipret.State        as Config
import           Qi.Program.Config.Lang               (ConfigEff, s3Bucket)
import qualified Qi.Program.Gen.Lang                  as Gen
import qualified Qi.Program.Lambda.Lang               as Lbd
import qualified Qi.Program.Wiring.IO                 as IO


withConfig
  :: Eff '[ConfigEff, State Config] ()
  -> IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  Options{ appName, cmd } <- showHelpOnErrorExecParser optionsSpec

  let template =
          CF.render
        . snd
        . run
        . runState def{_namePrefix = appName}
        . Config.run
        $ configProgram

      config =
          snd
        . run
        . runState def{_namePrefix = appName}
        . Config.run
        $ do
          s3Bucket "app" -- always assume existance of the app bucket
          configProgram

  (`runReaderT` config) $ liftIO . IO.run "dispatcher" config $

    case cmd of
      CfRenderTemplate     -> Gen.say $ toS template
      CfDeploy             ->
        liftIO . IO.run "dispatcher" config $
              Gen.build
          >>= Gen.readFileLazy
          >>= deployApp template

      CfCreate             -> createCfStack
      CfUpdate             -> updateCfStack
      CfDescribe           -> describeCfStack
      CfDestroy            -> destroyCfStack $ pure ()

      CfCycle              ->
        liftIO . IO.run "dispatcher" config $
              Gen.build
          >>= Gen.readFileLazy
          >>= cycleStack template

      LbdUpdate -> updateLambdas
      LbdSendEvent name -> do
        arg <- Gen.getLine

        -- get lambda id from name
        let id = Lbd.getIdByName config name

        Gen.putStr =<< case getById config id of

          GenericLambda{ _lbdGenericLambdaProgram } ->
            case eitherDecode $ toS arg of
              Right evt ->
                encode <$> _lbdGenericLambdaProgram evt
              Left err ->
                reportBadArgument "Generic" arg err


          S3BucketLambda{ _lbdS3BucketLambdaProgram } ->
            case parseEither (S3Event.parse config) =<< eitherDecode (toS arg) of
              Right evt ->
                _lbdS3BucketLambdaProgram evt
              Left err ->
                reportBadArgument "S3" arg err


          CfCustomLambda{ _lbdCfCustomLambdaProgram } ->
            case eitherDecode (toS arg) :: Either [Char] CfCustomResourceEvent of
              Right evt ->
                _lbdCfCustomLambdaProgram evt
              Left err ->
                reportBadArgument "CfCustom" arg err


          CwEventLambda{ _lbdCwEventLambdaProgram } ->
            case eitherDecode (toS arg) :: Either [Char] CwEvent of
              Right evt ->
                _lbdCwEventLambdaProgram evt
              Left err ->
                reportBadArgument "Cw" arg err

      LbdLogs _ -> pass -- lambdaLogs name

  where
    reportBadArgument lbdType arg err = panic $ "Could not parse event: '" <> toS arg <> "', for lambda type: '" <> lbdType <> "' error was: '" <> toS err <> "'"


{-
    parseLambdaEvent ApiLambda{_lbdApiMethodLambdaProgram} eventJson =
      _lbdApiMethodLambdaProgram <$> (parseEither (ApiMethodEvent.parse config) =<< eitherDecode (toS eventJson))


    parseLambdaEvent DdbStreamLambda{_lbdDdbStreamLambdaProgram} eventJson =
      _lbdDdbStreamLambdaProgram <$> (eitherDecode (toS eventJson) :: Either [Char] DdbStreamEvent)

-}


