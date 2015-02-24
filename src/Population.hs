module Population(Population, Individual(Individual)) where

import Data.Set
import Genes

data Individual = Individual (DnaString, DnaString) deriving (Eq, Show)

type Population = Set Individual