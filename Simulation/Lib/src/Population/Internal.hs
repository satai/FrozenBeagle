module Population.Internal
    ( Population (Population, generation, individuals)
    , PopulationChange
    , Selection
    , Breeding
    , Mutation
    , Fitness
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
    , chosenPairs
    ) where

import           Data.List
import           Data.Random (randomElement)
import           Data.Random.Distribution.Bernoulli
import           Data.Random.Distribution.Uniform (integralUniform)
import           Data.Random.Extras
import           Data.Random.RVar

import           Expression
import           Genes
import           Individual
import           ListUtils
import           Phenotype
import           Sex
import           SimulationConstants

data Population = Population
    { generation  :: !Int
    , individuals :: ![Individual]
} deriving (Eq, Show)

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

    crossOverPoint <- integralUniform 1 (baseCount - 1)

    swapChromosomes <- boolBernoulli (0.5 :: Double)

    chooseFromM <- boolBernoulli (0.5 :: Double)
    chooseFromF <- boolBernoulli (0.5 :: Double)

    let
        -- segregation
        mdna = if chooseFromM then mdna1 else mdna2
        fdna = if chooseFromF then fdna1 else fdna2

        -- crossover
        d1 = crossover crossOverPoint mdna fdna
        d2 = crossover crossOverPoint fdna mdna

        newChromosomes = if swapChromosomes then (d1, d2) else (d2, d1)
        offspringPhenotype = expression s newChromosomes

    return $ Individual s currentGeneration newChromosomes offspringPhenotype

randomOffspring _ _ _ _ = error "should not happen - only a M and F can be parents in this order"

mate :: ExpressionStrategy -> Phenotype -> Int -> (Individual, Individual) -> RVar [Individual]
mate expression optimum g parents = do

    let
        (father@(Individual M _ (mdna1, mdna2) _), mother@(Individual F _ (fdna1, fdna2) _)) = parents
        fatherFitness = fitness optimum $ expression M (mdna1, mdna2)
        motherFitness = fitness optimum $ expression F (fdna1, fdna2)
        pairFitness = (fatherFitness + motherFitness) / 2.0

    areParents <- boolBernoulli pairFitness

    if areParents
    then
        do
            offspring <- randomOffspring expression g father mother
            return [offspring]
    else
        return []

pointMutationAllele :: Double -> Double -> Allele -> RVar Allele
pointMutationAllele probabilityNegativeDominance probabilityPleiotropic b = do
    shouldMutateAllele <- boolBernoulli probabilityAlleleMutation

    if shouldMutateAllele
        then randomAllele probabilityNegativeDominance probabilityPleiotropic
        else return b

pointMutationDnaString :: Double -> Double -> DnaString -> RVar DnaString
pointMutationDnaString probabilityNegativeDominance probabilityPleiotropic (DnaString s) = do
    bases <- mapM (pointMutationAllele probabilityNegativeDominance probabilityPleiotropic) s
    return $ DnaString bases

pointMutationIndividual :: Double -> Double -> Individual -> RVar Individual
pointMutationIndividual probabilityNegativeDominance probabilityPleiotropic i = do
    let (d1, d2) = chromosomes i

    d1' <- pointMutationDnaString probabilityNegativeDominance probabilityPleiotropic d1
    d2' <- pointMutationDnaString probabilityNegativeDominance probabilityPleiotropic d2

    return $ Individual (sex i) (birthGeneration i) (d1', d2') (phenotype i)

pointMutation :: Double -> Double -> Mutation
pointMutation probabilityNegativeDominance probabilityPleiotropic = mapM (pointMutationIndividual probabilityNegativeDominance probabilityPleiotropic)

panmictic :: ExpressionStrategy -> Phenotype -> Int -> Breeding
panmictic expression optimum g population = do
    pairs <- chosenPairs 1 population
    children <- mapM (mate expression optimum g) pairs
    return $ concat children

panmicticOverlap :: ExpressionStrategy -> Phenotype -> Int -> Breeding
panmicticOverlap expression optimum g population = do
   pairs <- chosenPairs 1 population
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
    return $ map snd $ filter (not . fst) $ zipCheck shouldDie population

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
