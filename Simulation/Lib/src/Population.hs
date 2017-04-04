{-# LANGUAGE DeriveGeneric #-}

module Population
    ( Population (Population, generation, individuals)
    , PopulationChange
    , Selection
    , Breeding
    , Mutation
    , Fitness
    , Individual (Individual, birthGeneration, sex, chromosomes, phenotype)
    , Sex (F, M)
    , DnaString
    , panmictic
    , panmicticOverlap
    , hardSelection
    , pointMutation
    , turbidostat
    , killOld
    ) where

import Population.Internal