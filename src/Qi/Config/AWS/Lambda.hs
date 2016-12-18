{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.Lambda where

import           Data.Default                 (Default, def)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as SHM
import           Data.Text                    (Text)
import           Stratosphere

import           Qi.Config.AWS.ApiGw          (ApiMethodEvent)
import           Qi.Config.AWS.CF             (CfEvent)
import           Qi.Config.AWS.Lambda.Profile (LambdaProfile)
import           Qi.Config.AWS.S3             (S3Event)
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface  (LambdaProgram)

data Lambda = S3BucketLambda {
    _lbdName                  :: Text
  , _lbdProfile               :: LambdaProfile
  , _lbdS3BucketLambdaProgram :: S3Event -> LambdaProgram ()
  }
  | ApiLambda {
    _lbdName                   :: Text
  , _lbdProfile                :: LambdaProfile
  , _lbdApiMethodLambdaProgram :: ApiMethodEvent -> LambdaProgram ()
  }
  | CfCustomLambda {
    _lbdName                  :: Text
  , _lbdProfile               :: LambdaProfile
  , _lbdCFCustomLambdaProgram :: CfEvent -> LambdaProgram ()
  }

makeLenses ''Lambda


data LambdaConfig = LambdaConfig {
    _lcLambdas :: HashMap LambdaId Lambda
  }

makeLenses ''LambdaConfig


instance Default LambdaConfig where
  def = LambdaConfig {
    _lcLambdas = SHM.empty
  }




