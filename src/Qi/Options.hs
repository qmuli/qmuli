{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Options (
    optionsSpec
  , Options (..)
  , Command (..)
  , CfCommand (..)
  , LbdCommand (..)
  , showHelpOnErrorExecParser
  ) where

import           Data.Default        (def)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Options.Applicative
import           Protolude           hiding (runState)



data Options = Options {
    appName :: Text
  , cmd     :: Command
  }

data Command =
    Cf CfCommand
  | Lbd LbdCommand

data CfCommand =
    CfTemplate
  | CfDeploy
  | CfCreate
  | CfUpdate
  | CfDescribe
  | CfDestroy
  | CfCycle

data LbdCommand =
    LbdUpdate
  | LbdSendEvent Text Text


optionsSpec :: ParserInfo Options
optionsSpec = info (helper <*> optionsParser) fullDesc

optionsParser :: Parser Options
optionsParser = Options
  <$> nameParser
  <*> hsubparser (cfCmd <> lbdCmd)


nameParser :: Parser Text
nameParser = textArg "APP_NAME"

-- | Creates a Text positional argument with the given name
textArg
  :: Text
  -> Parser Text
textArg = argument str . metavar . toS


cfCmd :: Mod CommandFields Command
cfCmd =
    command "cf"
  $ info cfCmdParser
  $ fullDesc <> progDesc "Perform operations on a single node"
  where
    cfCmdParser = Cf <$> hsubparser (cfTemplateCmd)

    cfTemplateCmd :: Mod CommandFields CfCommand
    cfTemplateCmd =
        command "template"
      $ info cfTemplateCmdParser
      $ fullDesc <> progDesc "Perform operations on a single node"
      where
        cfTemplateCmdParser = pure CfTemplate





lbdCmd :: Mod CommandFields Command
lbdCmd =
    command "lbd"
  $ info lbdUpdateCmdParser
  $ fullDesc <> progDesc "Perform operations on a single node"
  where
    lbdUpdateCmdParser = Lbd <$> hsubparser (lbdUpdateCmd)

    lbdUpdateCmd :: Mod CommandFields LbdCommand
    lbdUpdateCmd =
        command "update"
      $ info lbdUpdateCmdParser
      $ fullDesc <> progDesc "Perform operations on a single node"
      where
        lbdUpdateCmdParser = pure LbdUpdate



-- | A version of 'execParser' which shows full help on error.
--
-- The regular 'execParser' only prints usage on error, which doesn't
-- include the options, subcommands, or mention of the help switch
-- @--help@.
showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)



