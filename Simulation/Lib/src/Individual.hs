module Individual
    ( Individual (..)
    ) where

import Genes
import Phenotype
import Sex

data Individual = Individual
    { sex             :: !Sex
    , birthGeneration :: !Int
    , chromosomes     :: !(DnaString, DnaString)
    , phenotype       :: !Phenotype
    } deriving (Eq, Show, Ord)
