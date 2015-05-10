{-# LANGUAGE DeriveGeneric #-}

module Sex(Sex(M,F)) where

import GHC.Generics (Generic)
import Data.Hashable

data Sex = M | F deriving (Eq, Show, Ord, Enum, Generic)

instance Hashable Sex
