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

evolution :: Int -> EvolutionRules -> RVar Population -> RVar [Population]
evolution 0 _ _ = return []
evolution genToGen spec population = do
      p <- population
      let g = 1 + generation p

      let breedingForGeneration = breeding spec g
      let mutationForGeneration = mutation spec
      let selectionForGeneration = selection spec

      let stepAlgo = [breedingForGeneration, mutationForGeneration, selectionForGeneration]

      newIndividuals <- step stepAlgo $ individuals <$> population

      rest <- evolution (genToGen - 1) spec <$> return $ Population g newIndividuals
      return $ (p : rest)
