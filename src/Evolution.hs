module Evolution(EvolutionRules(EvolutionRules, mutation, breeding, selection), evolution) where

import Data.Random
import Data.Functor

import Population

data EvolutionRules = EvolutionRules {
    mutation :: Mutation,
    breeding :: Breeding,
    selection :: Selection
}

step :: [PopulationChange] -> RVar Population -> RVar Population
step ss p = foldl (>>=) p ss

evolution :: EvolutionRules -> Population -> [(Int, RVar Population)]
evolution spec population = evolve (0, return population)
     where
        mutationForGeneration = mutation spec
        breedingForGeneration = breeding spec
        selectionForGeneration = selection spec
        stepAlgo = [selectionForGeneration, breedingForGeneration, mutationForGeneration]
        evolve :: (Int, RVar Population) -> [(Int, RVar Population)]
        evolve (gen, pop) = ((gen, pop) : evolve (gen + 1, step stepAlgo pop))