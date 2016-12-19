module Evolution(EvolutionRules(EvolutionRules, mutation, breeding, selection, deaths), evolution) where

import Data.Random
import Data.Functor()

import Population
import Phenotype

data EvolutionRules = EvolutionRules {
    mutation :: [Mutation],
    breeding :: [Int -> Breeding],
    selection :: [Phenotype -> Selection],
    deaths :: [Int -> PopulationChange]
}

step :: [PopulationChange] -> RVar [Individual] -> RVar [Individual]
step changes population = foldl (>>=) population changes

optimumForGeneration :: Int -> Phenotype
optimumForGeneration g = Phenotype [1.0, 0.0, 0.0, 0.0] --fixme

evolution :: Int -> EvolutionRules -> RVar Population -> RVar [Population]
evolution 0 _ _ = return []
evolution genToGen spec population = do
      p <- population
      let g = 1 + generation p

      let breedingForGeneration = map (\f -> f g) $ breeding spec
      let mutationForGeneration = mutation spec
      let selectionForGeneration = map (\f -> f $ optimumForGeneration g) $ selection spec
      let deathForGeneration = map (\f -> f g) $ deaths spec

      let stepAlgo = breedingForGeneration ++ mutationForGeneration ++ selectionForGeneration ++ deathForGeneration

      newIndividuals <- step stepAlgo $ individuals <$> population

      rest <- evolution (genToGen - 1) spec <$> return $ Population g newIndividuals
      return $ (p : rest)
