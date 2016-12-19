{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.Lambda where

import           Control.Lens
import           Data.Default                (Default, def)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import           Stratosphere

import           Qi.Config.AWS.ApiGw         (ApiMethodEvent)
import           Qi.Config.AWS.CF            (CfEvent)
import           Qi.Config.AWS.S3            (S3Event)
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaProgram)

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


data LambdaConfig = LambdaConfig {
    _lcLambdas :: HashMap LambdaId Lambda
  }

instance Default LambdaConfig where
  def = LambdaConfig {
    _lcLambdas = SHM.empty
  }


data LambdaMemorySize =
    M128
  | M1536

instance Enum LambdaMemorySize where
  toEnum 128  = M128
  toEnum 1536 = M1536

  fromEnum M128  = 128
  fromEnum M1536 = 1536


data LambdaProfile = LambdaProfile {
    _lpMemorySize     :: LambdaMemorySize
  , _lpTimeoutSeconds :: Int
  }

instance Default LambdaProfile where
  def = LambdaProfile {
      _lpMemorySize     = M128
    , _lpTimeoutSeconds = 30
    }


makeLenses ''Lambda
makeLenses ''LambdaConfig
makeLenses ''LambdaProfile

