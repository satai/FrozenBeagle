{-# LANGUAGE DeriveGeneric #-}

module Sex
    ( Sex (M, F)
    ) where

import           Data.Hashable
import           GHC.Generics  (Generic)

data Sex = M | F deriving (Eq, Show, Ord, Enum, Generic)

instance Hashable Sex
