{-# LANGUAGE DeriveGeneric #-}

module Individual
    ( Sex
    , Individual (Individual, sex, birthGeneration, chromosomes, phenotype)
    ) where

import GHC.Generics (Generic)
import Genes
import Phenotype
import Sex

import Data.Hashable
import Data.Bits

data Individual = Individual
    { sex             :: !Sex
    , birthGeneration :: !Int
    , chromosomes     :: !(DnaString, DnaString)
    , phenotype       :: !Phenotype
    } deriving (Eq, Show, Ord, Generic)

instance Hashable Individual
  where
    hashWithSalt s i = s `xor` hash (sex i) `xor` hash (chromosomes i)
