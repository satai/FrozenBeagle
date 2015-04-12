{-# LANGUAGE DeriveGeneric #-}

module Population (Population(Population, individuals),
                   PopulationChange, Selection, Breeding, Mutation,
                   Individual(Individual, sex, chromosomes), Sex(F,M),DnaString,
                   males, females,
                   allSurvive, fittest, extinction, fairChance, hardSelection) where

import Data.List
import Data.Hashable
import GHC.Generics (Generic)
import Data.Functor
import Genes
import Data.Random.RVar
import Data.Random.Extras

data Sex = M | F deriving (Eq, Show, Ord, Enum, Generic)

instance Hashable Sex

data Individual = Individual {
                                sex :: Sex,
                                chromosomes :: (DnaString, DnaString)
                             } deriving (Eq, Show, Ord, Generic)

instance Hashable Individual

data Population = Population { individuals :: [Individual] } deriving (Eq, Show)

males :: Population -> [Individual]
males = filter ( (== M) . sex) . individuals

females :: Population -> [Individual]
females = filter ( (== F) . sex) . individuals

type PopulationChange = Population -> RVar Population

type Mutation = PopulationChange

type Breeding = PopulationChange

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
        fitnessComparator i1 i2 = fitness i1 `compare` fitness i2

fairChance :: Int -> Selection
fairChance newSize p = Population <$> sample newSize (individuals p)
