module Qi.Config.Types where

import           Protolude


data ResourceExistence = AlreadyExists | ShouldCreate
  deriving (Eq, Show)
