module Qi.AWS.Logger where

import           Data.Aeson                   (FromJSON, ToJSON, Value (..),
                                               decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Builder as Build
import           Protolude
import           Qi.AWS.Types                 (MkAwsLogger)
import           System.IO                    (BufferMode (LineBuffering),
                                               hSetBuffering, stderr, stdout)


mkLambdaLogger
  :: MkAwsLogger
mkLambdaLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \lvl b ->
    hPutStrLn stderr . encode $ object ["level" .= String (show lvl), "message" .= String (toS $ Build.toLazyByteString b)]


mkCliLogger
  :: MkAwsLogger
mkCliLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \lvl b ->
    hPutStrLn stderr $ Build.toLazyByteString b
