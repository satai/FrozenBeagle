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

import           Data.List
import           Data.Random (randomElement)
import           Data.Random.Distribution.Bernoulli
import           Data.Random.Distribution.Uniform (integralUniform)
import           Data.Random.Extras
import           Data.Random.RVar
import           Expression
import           Genes
import           GHC.Generics (Generic)
import           Individual
import           Phenotype
import           Sex

data Population = Population
    { generation  :: !Int
    , individuals :: ![Individual]
} deriving (Eq, Show, Generic)

males :: [Individual] -> [Individual]
males = filter ( (== M) . sex)

females :: [Individual] -> [Individual]
females = filter ( (== F) . sex)

type PopulationChange = [Individual] -> RVar [Individual]

type Mutation = PopulationChange

type Breeding = PopulationChange

chosenPairs :: Int -> [Individual] -> RVar [(Individual, Individual)]
chosenPairs fraction population = do
    let numOfPairsPerGen = length population `div` fraction

    m <- shuffle $ males population
    f <- shuffle $ females population

    return $ take numOfPairsPerGen $ zip m f


randomOffspring :: ExpressionStrategy -> Int -> Individual -> Individual -> RVar Individual
randomOffspring expression currentGeneration (Individual M _ (mdna1, mdna2) _) (Individual F _ (fdna1, fdna2) _) = do
    s <- randomElement [M, F]

    let baseCount = length $ genes mdna1

    crossOverPoint1 <- integralUniform 1 (baseCount - 1)
    crossOverPoint2 <- integralUniform 1 (baseCount - 1)

    let
        d1 = crossover crossOverPoint1 mdna1 fdna1
        d2 = crossover crossOverPoint2 mdna2 fdna2
        offspringPhenotype = expression s (d1, d2)

    return $ Individual s currentGeneration (d1, d2) offspringPhenotype

randomOffspring _ _ _ _ = error "should not happen - only a M and F can be parents in this order"

maximumCountOfOffspring :: Int
maximumCountOfOffspring = 6

mate :: ExpressionStrategy -> Phenotype -> Int -> (Individual, Individual) -> RVar [Individual]
mate expression optimum g parents = do

    let
        (father@(Individual M _ (mdna1, mdna2) _), mother@(Individual F _ (fdna1, fdna2) _)) = parents
        fatherFitness = fitness optimum $ expression M (mdna1, mdna2)
        motherFitness = fitness optimum $ expression F (fdna1, fdna2)
        pairFitness = (fatherFitness + motherFitness) / 2.0

        numOfOffspring = min maximumCountOfOffspring $ floor $ pairFitness

    randOffspring1 <- randomOffspring expression g father mother
    randOffspring2 <- randomOffspring expression g father mother
    randOffspring3 <- randomOffspring expression g father mother
    randOffspring4 <- randomOffspring expression g father mother
    randOffspring5 <- randomOffspring expression g father mother
    randOffspring6 <- randomOffspring expression g father mother

    return $ take numOfOffspring [randOffspring1, randOffspring2, randOffspring3, randOffspring4, randOffspring5, randOffspring6]

probabilityIndividualMutation :: Float
probabilityIndividualMutation = 0.01

probabilityBasisMutation :: Float
probabilityBasisMutation = 0.02

pointMutationBasis :: Basis -> RVar Basis
pointMutationBasis b = do
    shouldMutateBasis <- boolBernoulli probabilityBasisMutation

    if shouldMutateBasis
        then choice [ G1
                    , G2
                    , G3
                    ]
        else return b

pointMutationDnaString :: DnaString -> RVar DnaString
pointMutationDnaString  (DnaString s) = do
    bases <- mapM pointMutationBasis s
    return $ DnaString bases

pointMutationIndividual :: ExpressionStrategy -> Individual -> RVar Individual
pointMutationIndividual expression i = do

        shouldMutateIndividual <- boolBernoulli probabilityIndividualMutation

        if shouldMutateIndividual
            then do
                let (d1, d2) = chromosomes i

                d1' <- pointMutationDnaString d1
                d2' <- pointMutationDnaString d2

                let offspringPhenotype = expression (sex i) (d1', d2')

                return $ Individual (sex i) (birthGeneration i) (d1', d2') offspringPhenotype
            else
                return i

pointMutation :: ExpressionStrategy -> Mutation
pointMutation expression = mapM (pointMutationIndividual expression)

panmictic :: ExpressionStrategy -> Phenotype -> Int -> Breeding
panmictic expression optimum g population = do
    pairs <- chosenPairs 1 population
    children <- mapM (mate expression optimum g) pairs
    return $ concat children

panmicticOverlap :: ExpressionStrategy -> Phenotype -> Int -> Breeding
panmicticOverlap expression optimum g population = do
   pairs <- chosenPairs 8 population
   children <- mapM (mate expression optimum g) pairs
   return $ population ++ concat children

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
