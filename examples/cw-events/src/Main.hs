{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import           Data.Default                (def)
import           Protolude
import           Qi                          (withConfig)
import           Qi.Config.AWS.CW            (CwEventsRuleProfile (ScheduledEventProfile))
import           Qi.Program.Config.Interface (ConfigProgram, cwEventLambda)
import           Qi.Program.Lambda.Interface (CwLambdaProgram, say)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)"

      void $ cwEventLambda "myEventLambda" ruleProfile eventLambda def

    eventLambda
      :: CwLambdaProgram
    eventLambda _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      say "tick"
      pure "all done!"
