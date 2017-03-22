module Evolution
    (
       EvolutionRules (EvolutionRules, mutation, breeding, selection, deaths, expression, optimumForGeneration)
    , evolution
    ) where

import           Data.Random
import           Data.Functor ()

import           Population
import           Phenotype
import           Expression

data EvolutionRules =
       EvolutionRules
           { mutation             :: [Mutation]
           , breeding             :: [Phenotype -> Int -> Breeding]
           , selection            :: [Phenotype -> Selection]
           , deaths               :: [Int -> PopulationChange]
           , expression           :: ExpressionStrategy
           , optimumForGeneration :: Int -> Phenotype
           }

step :: [PopulationChange] -> RVar [Individual] -> RVar [Individual]
step changes population = foldl (>>=) population changes

evolution :: Int -> EvolutionRules -> RVar Population -> RVar [Population]
evolution 0 _ _ = return []
evolution genToGen spec population = do
    p <- population

    let g = 1 + generation p
    let todayOptimum = optimumForGeneration spec g

    let breedingForGeneration = map (\f -> f todayOptimum g) $ breeding spec
    let mutationForGeneration = mutation spec
    let selectionForGeneration = map (\f -> f todayOptimum) $ selection spec
    let deathForGeneration = map (\f -> f g) $ deaths spec

    let stepAlgo = breedingForGeneration ++
                   mutationForGeneration ++
                   selectionForGeneration ++
                   deathForGeneration

    newIndividuals <- step stepAlgo $ individuals <$> population

    rest <- evolution (genToGen - 1) spec <$> return $ Population g newIndividuals
    return (p : rest)
