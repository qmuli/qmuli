{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad                   (void)
import           Data.Aeson
import           Data.Default                    (def)
import qualified Data.Text                       as T
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan
import           Prelude                         hiding (scan)

import           Qi                              (withConfig)
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..))
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram,
                                                  ddbStreamLambda, ddbTable)
import           Qi.Program.Lambda.Interface     (DdbStreamLambdaProgram, say)
import           Qi.Util                         (success)
import           Qi.Util.DDB


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      inputThingsTable <- ddbTable "things" (DdbAttrDef "name" S) def

      void $ ddbStreamLambda "ddbStreamLambda" inputThingsTable handler def

    handler
      :: DdbStreamLambdaProgram
    handler event = do
      say $ T.concat ["got stream event: ", T.pack $ show event]
      success "successfully completed lambda"
