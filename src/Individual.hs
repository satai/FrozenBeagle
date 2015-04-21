{-# LANGUAGE DeriveGeneric #-}

module Individual(Sex, Individual(Individual, sex, chromosomes, phenotype)) where

import GHC.Generics (Generic)
import Genes
import Phenotype
import Sex

data Individual = Individual {
                                sex :: Sex,
                                chromosomes :: (DnaString, DnaString),
                                phenotype :: Phenotype
                             } deriving (Eq, Show, Ord, Generic)