module Evolution(EvolutionRules(EvolutionRules, mutation, breeding, selection), evolution) where

import Data.Random
import Data.Functor()

import Population

data EvolutionRules = EvolutionRules {
    mutation :: Mutation,
    breeding :: Breeding,
    selection :: Selection
}

step :: [PopulationChange] -> RVar Population -> RVar Population
step ss p = foldl (>>=) p ss

evolution :: EvolutionRules -> RVar Population -> [RVar Population]
evolution spec population = population : evolution spec tng
     where
        mutationForGeneration = mutation spec
        breedingForGeneration = breeding spec
        selectionForGeneration = selection spec
        stepAlgo = [breedingForGeneration, mutationForGeneration, selectionForGeneration]
        tng = step stepAlgo population
