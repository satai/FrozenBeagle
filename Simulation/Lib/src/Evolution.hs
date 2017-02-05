module Evolution(EvolutionRules(EvolutionRules, mutation, breeding, selection, deaths, expression), evolution) where

import Data.Random
import Data.Functor()

import Population
import Phenotype
import Individual
import Expression

data EvolutionRules = EvolutionRules {
    mutation :: [Mutation],
    breeding :: [Double -> Double -> Phenotype -> Int -> Breeding],
    selection :: [Phenotype -> Selection],
    deaths :: [Int -> PopulationChange],
    expression :: ExpressionStrategy
}

step :: [PopulationChange] -> RVar [Individual] -> RVar [Individual]
step changes population = foldl (>>=) population changes

optimumForGeneration :: Int -> Phenotype
optimumForGeneration g = Phenotype [1.0, 0.0, 0.0, 0.0] --fixme

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev xs = sqrt $ summedElements / count
  where
    avg = average xs
    count = fromIntegral $ length xs
    summedElements = sum (map (\x -> (x - avg) ^ 2) xs)

evolution :: Int -> EvolutionRules -> RVar Population -> RVar [Population]
evolution 0 _ _ = return []
evolution genToGen spec population = do
      p <- population

      let g = 1 + generation p
      let todaysOptimum = optimumForGeneration g
      let actualPhenotypes =  map phenotype $ individuals p
      let avgFitness = average $ map (fitness todaysOptimum) actualPhenotypes
      let dev = stdDev $ map (fitness todaysOptimum) actualPhenotypes

      let breedingForGeneration = map (\f -> f avgFitness dev todaysOptimum g) $ breeding spec
      let mutationForGeneration = mutation spec
      let selectionForGeneration = map (\f -> f todaysOptimum) $ selection spec
      let deathForGeneration = map (\f -> f g) $ deaths spec

      let stepAlgo = breedingForGeneration ++ mutationForGeneration ++ selectionForGeneration ++ deathForGeneration

      newIndividuals <- step stepAlgo $ individuals <$> population

      rest <- evolution (genToGen - 1) spec <$> return $ Population g newIndividuals
      return $ (p : rest)
