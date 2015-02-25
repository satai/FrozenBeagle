module Population(Population(Population), Individual(Individual), allSurvive) where

import Data.Set
import Genes

data Individual = Individual (DnaString, DnaString) deriving (Eq, Show)

instance Ord Individual where
    (Individual i1) `compare` (Individual i2) = i1 `compare` i2

data Population = Population (Set Individual) deriving (Eq, Show)

type Selection = Population -> Population

allSurvive :: Selection
allSurvive = id