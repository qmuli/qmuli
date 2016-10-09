{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State.Strict           (runState)
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Default                         (def)
import           System.Environment                   (getArgs)

import           Assets                               (config)
import           Qi.Config.CF                         (render)
import           Qi.Program.Config.Interpreters.Build (interpret)


main :: IO ()
main = do

  [cmd] <- getArgs
  case cmd of
    "cf" -> do
      LBS.putStr json

    x -> putStrLn $ "unexpected command: '" ++ x ++ "'"

  where
    json = render . snd . (`runState` def) $ interpret config





