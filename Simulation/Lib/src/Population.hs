{-# LANGUAGE DeriveGeneric #-}

module Population
    ( Population (Population, generation, individuals)
    , PopulationChange
    , Selection
    , Breeding
    , Mutation
    , Fitness
    , Individual (Individual, birthGeneration, sex, chromosomes, phenotype)
    , Sex(F, M)
    , DnaString
    , males
    , females
    , pointMutation
    , panmictic
    , panmicticOverlap
    , allSurvive
    , fittest
    , extinction
    , fairChance
    , hardSelection
    , turbidostat
    , killOld
    ) where

import           Data.Bits
import           Data.Hashable
import           Data.List
import           Data.Random (sampleState)
import           Data.Random.Distribution.Bernoulli
import           Data.Random.Extras
import           Data.Random.RVar
import           Expression
import           Genes
import           GHC.Generics (Generic)
import           Individual
import           Phenotype
import           Sex
import           System.Random

data Population = Population
    { generation  :: !Int
    , individuals :: ![Individual]
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
chosenPairs population = take numOfPairsPerGen $ zip m f
  where
    seed = hash population
    gen = mkStdGen seed
    m' = shuffle $ males population
    f' = shuffle $ females population
    (m, gen') = sampleState m' gen
    (f, _   ) = sampleState f' gen'
    numOfPairsPerGen = length population `div` 8


randomOffspring :: ExpressionStrategy -> Int -> Individual -> Individual -> Int -> Individual
randomOffspring expression currentGeneration (Individual M _ (mdna1, mdna2) _) (Individual F _ (fdna1, fdna2) _) seed = Individual s currentGeneration (d1, d2) $ expression s (d1, d2)
 where
    gen = mkStdGen seed
    (s', gen') = next gen

    s = if odd s' then M else F

    baseCount = length $ genes mdna1

    (crossOverPoint1, gen'') = randomR (1, baseCount - 1) gen'
    (crossOverPoint2, _) = randomR (1, baseCount - 1) gen''

    d1 = crossover crossOverPoint1 mdna1 fdna1
    d2 = crossover crossOverPoint2 mdna2 fdna2

randomOffspring _ _ _ _ _ = error "should not happen"

maximumCountOfOffspring :: Int
maximumCountOfOffspring = 6

mate :: ExpressionStrategy -> Phenotype -> Int -> (Individual, Individual) -> [Individual]
mate expression optimum g (father@(Individual M _ (mdna1, mdna2) _), mother@(Individual F _ (fdna1, fdna2) _)) = take numOfOffspring [randOffspring1, randOffspring2, randOffspring3, randOffspring4, randOffspring5, randOffspring6]
  where
    seed = hash optimum `xor` hash father `xor` hash mother `xor` g

    fatherFitness = fitness optimum $ expression M (mdna1, mdna2)
    motherFitness = fitness optimum $ expression F (fdna1, fdna2)
    pairFitness = (fatherFitness + motherFitness) / 2.0

    numOfOffspring = min maximumCountOfOffspring $ floor $ log $ 60 * pairFitness
    randOffspring1 = randomOffspring expression g father mother seed
    randOffspring2 = randomOffspring expression g father mother (seed + 1)
    randOffspring3 = randomOffspring expression g father mother (seed + 2)
    randOffspring4 = randomOffspring expression g father mother (seed + 3)
    randOffspring5 = randomOffspring expression g father mother (seed + 4)
    randOffspring6 = randomOffspring expression g father mother (seed + 5)

mate _ _ _ _ = error "Should not happen"

probabilityIndividualMutation :: Float
probabilityIndividualMutation = 0.01

probabilityBasisMutation :: Float
probabilityBasisMutation = 0.04

pointMutationBasis :: Basis -> RVar Basis
pointMutationBasis b = do
    shouldMutateBasis <- boolBernoulli probabilityBasisMutation

    if shouldMutateBasis
        then choice [G1, G2, G3, G4, G5]
        else return b

pointMutationDnaString :: DnaString -> RVar DnaString
pointMutationDnaString  (DnaString s) = do
    bases <- mapM pointMutationBasis s
    return $ DnaString bases

pointMutationIndividual :: ExpressionStrategy -> Individual -> RVar Individual
pointMutationIndividual expression i = do

        shoudMutateIndividual <- boolBernoulli probabilityIndividualMutation

        if shoudMutateIndividual
            then do
                let (d1, d2) = chromosomes i

                d1' <- pointMutationDnaString d1
                d2' <- pointMutationDnaString d2

                return $ Individual (sex i) (birthGeneration i) (d1', d2') $ expression (sex i) (d1', d2')
            else
                return i

pointMutation :: ExpressionStrategy -> Mutation
pointMutation expression = mapM (pointMutationIndividual expression)

panmictic :: ExpressionStrategy -> Phenotype -> Int -> Breeding
panmictic expression optimum g population = return $ concat children
  where
    children :: [[Individual]]
    children = map (mate expression optimum g) $ chosenPairs population

panmicticOverlap :: ExpressionStrategy -> Phenotype -> Int -> Breeding
panmicticOverlap expression optimum g population = return $ population ++ concat children
  where
    children :: [[Individual]]
    children = map (mate expression optimum g) $ chosenPairs population

type Selection = PopulationChange

allSurvive :: Selection
allSurvive = return

extinction :: Selection
extinction = return . const []

type Fitness = Phenotype -> Double

hardSelection :: Fitness -> Double -> Selection
hardSelection fitness' threshold = return . filterSurvivors
  where
    filterSurvivors :: [Individual] -> [Individual]
    filterSurvivors = filter ((> threshold) . fitness' . phenotype)

turbidostat :: Double -> Double -> PopulationChange
turbidostat k4 k5 population = do
    let actualSize = length population
    let turbidostatProbability = min 0.9 $ k4 * fromIntegral (actualSize * actualSize) + k5
    shouldDie <- mapM (\_ -> boolBernoulli turbidostatProbability) [1 .. actualSize]
    return $ map snd $ filter (not . fst) $ zip shouldDie population

killOld :: Int -> Int -> PopulationChange
killOld ageToDie currentGeneration population = do
    let youngEnough i = birthGeneration i + ageToDie > currentGeneration
    return $ filter youngEnough population

fittest :: Int -> Fitness -> Selection
fittest newSize fitness' is = return survivors
  where
    survivors :: [Individual]
    survivors = take newSize $ sortBy fitnessComparator is
    fitnessComparator :: Individual ->Individual -> Ordering
    fitnessComparator i1 i2 = fitness' (phenotype i2) `compare` fitness' (phenotype i1)

fairChance :: Int -> Selection
fairChance = sample
