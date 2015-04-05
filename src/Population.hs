module Population (Population(Population, individuals), Individual(Individual, sex, chromosomes), Selection, Sex(F,M),
                   males, females,
                   allSurvive, fittest, extinction, fairChance) where

import Data.List
import Data.Functor
import Genes
import Data.Random.RVar
import Data.Random.Extras

data Sex = M | F deriving (Eq, Show, Ord)

data Individual = Individual {
                                sex :: Sex,
                                chromosomes :: (DnaString, DnaString)
                             } deriving (Eq, Show, Ord)

data Population = Population { individuals :: [Individual] } deriving (Eq, Show)

males :: Population -> [Individual]
males = filter ( (== M) . sex) . individuals

females :: Population -> [Individual]
females = filter ( (== F) . sex) . individuals

type Selection = Population -> RVar Population

allSurvive :: Selection
allSurvive = return

extinction :: Selection
extinction _ = return $ Population []

type Fitness = Individual -> Double

fittest :: Int -> Fitness-> Selection
fittest newSize fitness (Population p) = return $ Population survivors
    where
        survivors = take newSize $ sortBy fitnessComparator p
        fitnessComparator i1 i2 = fitness i1 `compare` fitness i2

fairChance :: Int -> Selection
fairChance newSize p = Population <$> sample newSize (individuals p)