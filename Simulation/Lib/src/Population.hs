{-# LANGUAGE DeriveGeneric #-}

module Population (Population(Population, generation, individuals),
                   PopulationChange, Selection, Breeding, Mutation, Fitness,
                   Individual(Individual, birthGeneration, sex, chromosomes, phenotype), Sex(F,M),DnaString,
                   males, females,
                   panmictic, panmicticOverlap,
                   allSurvive, fittest, extinction, fairChance, hardSelection) where

import Data.List
import Data.Hashable
import GHC.Generics (Generic)
import Data.Random.Extras
import Data.Random(sampleState)
import Data.Random.RVar
import System.Random
import Genes
import Sex
import Individual
import Phenotype
import Expression

data Population = Population {
      generation :: Int,
      individuals :: [Individual]
} deriving (Eq, Show, Generic)

instance Hashable Population

males :: [Individual] -> [Individual]
males = filter ( (== M) . sex)

females :: [Individual] -> [Individual]
females = filter ( (== F) . sex)

type PopulationChange = [Individual] -> RVar [Individual]

type Mutation = PopulationChange

type Breeding = PopulationChange

chosenPairs :: [Individual] -> [(Individual, Individual)]
chosenPairs population = zip m f
    where seed = hash population
          gen = mkStdGen seed
          m' = shuffle $ males population
          f' = shuffle $ females population
          (m, gen') = sampleState m' gen
          (f, _   ) = sampleState f' gen'

mate :: ExpressionStrategy -> Int -> (Individual, Individual) -> [Individual]
mate expression g (Individual M _ (mdna1, mdna2) _, Individual F _ (fdna1, fdna2) _) = [son1, daughter1, son2, daughter2]
        where
            s1d1 = crossover 5 mdna1 fdna1
            s1d2 = crossover 5 fdna2 mdna2
            d1d1 = crossover 4 mdna1 fdna1
            d1d2 = crossover 4 fdna2 mdna2

            son1 = Individual M g (s1d1, s1d2) $ expression M (s1d1, s1d2)
            daughter1 = Individual F g (d1d1, d1d2) $ expression F (d1d1, d1d2)

            s2d1 = crossover 7 mdna1 fdna1
            s2d2 = crossover 7 fdna2 mdna2
            d2d1 = crossover 8 mdna1 fdna1
            d2d2 = crossover 8 fdna2 mdna2

            son2 = Individual M g (s2d1, s2d2) $ expression M (s2d1, s2d2)
            daughter2 = Individual F g (d2d1, d2d2) $ expression F (d2d1, d2d2)
            --FIXME totaly wrong

mate _ _ _ = error "Should not happen"

panmictic :: ExpressionStrategy -> Int -> Breeding
panmictic expression g population = return $ concat children
        where
          children :: [[Individual]]
          children = map (mate expression g) $ chosenPairs population

panmicticOverlap :: ExpressionStrategy -> Int -> Breeding
panmicticOverlap expression g population = return $ population ++ concat children
        where
          children :: [[Individual]]
          children = map (mate expression g) $ chosenPairs population

type Selection = PopulationChange

allSurvive :: Selection
allSurvive = return

extinction :: Selection
extinction = return . const []

type Fitness = Phenotype -> Double

hardSelection :: Fitness -> Double -> Selection
hardSelection fitness treshold = return . filterSurvivors
        where
            filterSurvivors :: [Individual] -> [Individual]
            filterSurvivors = filter ((>treshold) . fitness . phenotype)

fittest :: Int -> Fitness -> Selection
fittest newSize fitness is = return survivors
    where
        survivors :: [Individual]
        survivors = take newSize $ sortBy fitnessComparator is
        fitnessComparator :: Individual ->Individual -> Ordering
        fitnessComparator i1 i2 = fitness (phenotype i2) `compare` fitness (phenotype i1)

fairChance :: Int -> Selection
fairChance newSize is = sample newSize is
