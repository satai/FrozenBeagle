{-# LANGUAGE DeriveGeneric #-}

module Sex
    ( Sex (..)
    ) where

import           GHC.Generics  (Generic)

data Sex = M | F deriving (Eq, Show, Ord, Enum, Generic)
