module Evolution(EvolutionRules(EvolutionRules, mutation, breeding, selection), evolution) where

import Data.Random
import Data.Functor()

import Population

data EvolutionRules = EvolutionRules {
    mutation :: Mutation,
    breeding :: Breeding,
    selection :: Selection
}

step :: [PopulationChange] -> RVar [Individual] -> RVar [Individual]
step changes population = foldl (>>=) population changes

evolution :: EvolutionRules -> RVar Population -> [RVar Population]
evolution spec population = population : evolution spec tng
     where
        mutationForGeneration = mutation spec
        breedingForGeneration = breeding spec
        selectionForGeneration = selection spec
        stepAlgo = [breedingForGeneration, mutationForGeneration, selectionForGeneration]

        tng :: RVar Population
        tng = do
          p <- population
          let g = generation p
          let newIndividuals = step stepAlgo $ individuals <$> population
          Population (g + 1) <$> newIndividuals
