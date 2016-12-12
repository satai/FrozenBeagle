module Evolution(EvolutionRules(EvolutionRules, mutation, breeding, selection), evolution) where

import Data.Random
import Data.Functor()

import Population

data EvolutionRules = EvolutionRules {
    mutation :: Mutation,
    breeding :: Int -> Breeding,
    selection :: Selection
}

step :: [PopulationChange] -> RVar [Individual] -> RVar [Individual]
step changes population = foldl (>>=) population changes

evolution :: EvolutionRules -> RVar Population -> [RVar Population]
evolution spec population = population : evolution spec tng
     where

        tng :: RVar Population
        tng = do
          p <- population
          let g = 1 + generation p

          let breedingForGeneration = breeding spec g
          let mutationForGeneration = mutation spec
          let selectionForGeneration = selection spec

          let stepAlgo = [breedingForGeneration, mutationForGeneration, selectionForGeneration]
          let newIndividuals = step stepAlgo $ individuals <$> population
          Population g  <$> newIndividuals
