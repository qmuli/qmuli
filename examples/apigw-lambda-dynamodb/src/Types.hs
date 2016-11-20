{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where


import           Control.Monad    ((<=<))
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Generics

data Thing = Thing {
    name  :: Text
  , shape :: Text
  , size  :: Int
} deriving (Generic, Show)

instance ToJSON Thing where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Thing where


newtype DdbThing = DdbThing {unDdbThing :: Thing}

instance ToJSON DdbThing where
  toJSON (DdbThing Thing{name, shape, size}) = asObject
    [
      "name"  .= asString name
    , "shape" .= asString shape
    , "size"  .= asNumber size
    ]

asNumber
  :: Show a
  => a
  -> Value
asNumber n = object ["N" .= show n]

asString
  :: ToJSON a
  => a
  -> Value
asString s = object ["S" .= s]

asObject
  :: [(Text, Value)]
  -> Value
asObject o = object ["M" .= object o]


instance FromJSON DdbThing where
  parseJSON (Object v) = do
    obj <- fromObject v
    DdbThing <$> (
          Thing
            <$> fromStringProp "name" obj
            <*> fromStringProp "shape" obj
            <*> fromNumberProp "size" obj
        )

    where
      fromNumber = fmap (read . T.unpack) . (.: "N")
      fromNumberProp name = fromNumber <=< (.: name)

      fromString = (.: "S")
      fromStringProp name = fromString <=< (.: name)

      fromObject = (.: "M")
      fromObjectProp name = fromObject <=< (.: name)

  parseJSON invalid = typeMismatch "DdbThing" invalid



