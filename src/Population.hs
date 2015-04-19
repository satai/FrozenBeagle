{-# LANGUAGE DeriveGeneric #-}

module Population (Population(Population, individuals),
                   PopulationChange, Selection, Breeding, Mutation,
                   Individual(Individual, sex, chromosomes), Sex(F,M),DnaString,
                   males, females,
                   panmictic,
                   allSurvive, fittest, extinction, fairChance, hardSelection) where

import Data.List
import Data.Hashable
import GHC.Generics (Generic)
import Data.Random.Extras
import Data.Random(sampleState)
import Data.Random.RVar
import System.Random
import Data.Functor
import Genes
import Debug.Trace

data Sex = M | F deriving (Eq, Show, Ord, Enum, Generic)

instance Hashable Sex

data Individual = Individual {
                                sex :: Sex,
                                chromosomes :: (DnaString, DnaString)
                             } deriving (Eq, Show, Ord, Generic)

instance Hashable Individual

data Population = Population { individuals :: [Individual] } deriving (Eq, Show, Generic)

instance Hashable Population

males :: Population -> [Individual]
males = filter ( (== M) . sex) . individuals

females :: Population -> [Individual]
females = filter ( (== F) . sex) . individuals

type PopulationChange = Population -> RVar Population

type Mutation = PopulationChange

type Breeding = PopulationChange

chosenPairs :: Population -> [(Individual, Individual)]
chosenPairs population = zip m f
    where seed = hash population
          gen = mkStdGen seed
          m' = shuffle $ males population
          f' = shuffle $ females population
          (m, gen') = sampleState m' gen
          (f, _   ) = sampleState f' gen'

mate :: (Individual, Individual) -> [Individual]
mate (Individual M (mdna1, mdna2), Individual F (fdna1, fdna2)) = [son1, daughter1, son2, daughter2]
        where
            s1d1 = crossover 5 mdna1 fdna1
            s1d2 = crossover 5 fdna2 mdna2
            d1d1 = crossover 4 mdna1 fdna1
            d1d2 = crossover 4 fdna2 mdna2

            son1 = Individual M (s1d1, s1d2)
            daughter1 = Individual F (d1d1, d1d2)

            s2d1 = crossover 7 mdna1 fdna1
            s2d2 = crossover 7 fdna2 mdna2
            d2d1 = crossover 8 mdna1 fdna1
            d2d2 = crossover 8 fdna2 mdna2

            son2 = Individual M (s2d1, s2d2)
            daughter2 = Individual F (d2d1, d2d2)
            --FIXME totaly wrong

panmictic :: Breeding
panmictic population = return $ Population $ concat children
        where
          children = map mate $ chosenPairs population

type Selection = PopulationChange

allSurvive :: Selection
allSurvive = return

extinction :: Selection
extinction = return . (const $ Population [])

hardSelection :: Fitness -> Double -> Selection
hardSelection fitness treshold = return . Population . filterSurvivors . individuals
        where
            filterSurvivors :: [Individual] -> [Individual]
            filterSurvivors = filter ((>treshold) . fitness)

type Fitness = Individual -> Double

fittest :: Int -> Fitness-> Selection
fittest newSize fitness (Population p) = return $ Population survivors
    where
        survivors = take newSize $ sortBy fitnessComparator p
        fitnessComparator i1 i2 = fitness i2 `compare` fitness i1

fairChance :: Int -> Selection
fairChance newSize p = Population <$> sample newSize (individuals p)
