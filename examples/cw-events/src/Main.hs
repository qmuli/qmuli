{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import           Data.Default                (def)
import           Data.Text                   (pack)

import           Qi                          (withConfig)
import           Qi.Config.AWS.CW            (CwEvent, CwEventsRuleProfile (..))
import           Qi.Config.AWS.Lambda        (LambdaMemorySize (..),
                                              lpMemorySize)
import           Qi.Program.Config.Interface (ConfigProgram, cwEventLambda)
import           Qi.Program.Lambda.Interface (LambdaProgram, say)

main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)"

      void $ cwEventLambda "myEventLambda" ruleProfile eventLambda $
        def & lpMemorySize .~ M1536

    eventLambda
      :: CwEvent -> LambdaProgram ()
    eventLambda event = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      say "hello there!"


