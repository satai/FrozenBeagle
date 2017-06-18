module Simulation.Internal
    ( computeSimulation
    , AnalysisParameters(..)
    , turbidostatCoefficientsForPopulationSize
    , optimumCalculation
    , randomOptimum
    , collapse
    , randomPopulation
    , randomRules
    , average
    , stdDev
    , allTheSame
    , almostAllTheSame
    ) where

import           SimulationConstants
import           Evolution
import           Expression
import           Genes
import           Individual
import           ListUtils
import           Phenotype
import           Population
import           Schema

import           Control.Monad
import           Data.List
import           Data.MultiSet                    (fromList, toOccurList)

import           Data.Random
import           Data.Random.Distribution.Bernoulli
import           Data.Random.Distribution.Normal
import           Data.Random.Distribution.Uniform
import           Data.Random.Extras hiding (shuffle)
import           System.Random

import Debug.Trace

data AnalysisParameters = AnalysisParameters
    { separatedGenerations           :: Bool
    , hardSelectionThreshold         :: Double
    , populationSize                 :: Int
    , optimumChange                  :: [(Double, Double, Double)]
    , maxAge                         :: Int
    , countOfBases                   :: Int
    , countOfPleiotropicRules        :: Int
    , countOfEpistaticRules          :: Int
    , countOfComplicatedRules        :: Int
    , countOfDominantRules           :: Int
    , countOfNegativeDominantRules   :: Int
    , countOfPositiveDominantRules   :: Int
    , seed                           :: Int
    } deriving Show

randomPopulation :: Int -> ExpressionStrategy -> Int -> RVar Population
randomPopulation count expressionStrategy baseCount = Population 0 <$> randomIndividuals count expressionStrategy baseCount

randomIndividuals :: Int -> ExpressionStrategy -> Int -> RVar [Individual]
randomIndividuals count expressionStrategy baseCount = replicateM count $ randomIndividual baseCount expressionStrategy

randomIndividual :: Int -> ExpressionStrategy -> RVar Individual
randomIndividual baseCount expressionStrategy = do
    individualsSex <- randomSex
    chs <- randomChromosomes baseCount
    return $ Individual individualsSex 0 chs $ expressionStrategy individualsSex chs

randomSex :: RVar Sex
randomSex = choice [F, M]

randomChromosomes :: Int -> RVar (DnaString, DnaString)
randomChromosomes baseCount = do
    dna1 <- randomDnaString baseCount
    dna2 <- randomDnaString baseCount
    return (dna1, dna2)

randomDnaString :: Int -> RVar DnaString
randomDnaString baseCount = DnaString <$> replicateM baseCount randomInitAllele

randomInitAllele :: RVar Allele
randomInitAllele = do
    isZero <- boolBernoulli (0.99 :: Double)

    if isZero
        then return $ Allele (Phenotype  [0.0, 0.0, 0.0, 0.0]) (Phenotype  [0.0, 0.0, 0.0, 0.0])
        else randomAllele

avgFitness :: (Int -> Phenotype) -> Int -> Population -> Double
avgFitness generationOptimum generationNumber = avgFitnessForGeneration (generationOptimum generationNumber)

homozygotness :: (Int -> Phenotype) -> Int -> Population -> Double
homozygotness _ _ population =
    let
        is = individuals population
        chs = map chromosomes is
        ps = zip (concatMap (genes . fst) chs) (concatMap (genes . snd) chs)
        homos = filter (uncurry (==)) ps
    in
        fromIntegral (length homos) / fromIntegral (length ps)



avgFitnessForGeneration :: Phenotype -> Population -> Double
avgFitnessForGeneration optimum (Population _ is)  = average $ map (fitness optimum . phenotype) is

stdDevFitness :: (Int -> Phenotype) -> Int -> Population -> Double
stdDevFitness generationOptimum generationNumber = stdDevFitnessForGeneration (generationOptimum generationNumber)

stdDevFitnessForGeneration :: Phenotype -> Population -> Double
stdDevFitnessForGeneration optimum (Population _ is)  = stdDev $ map (fitness optimum . phenotype) is

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame xs = all (== head xs) (tail xs)

almostAllTheSame :: (Ord a) => [a] -> Bool
almostAllTheSame [] = True
almostAllTheSame xs = (0.90 :: Double) * fromIntegral (length xs)  <= fromIntegral (maximum $ map snd $ toOccurList $ fromList xs)

polymorphism :: Population -> Double
polymorphism population = 1.0 - fromIntegral (length $ filter id same) / fromIntegral (length same)
  where
    chs = map chromosomes $ individuals population
    genesList = transpose $ map genes $ map fst chs ++ map snd chs
    same :: [Bool]
    same = map allTheSame genesList

almostPolymorphism :: Population -> Double
almostPolymorphism population = 1.0 - fromIntegral (length $ filter id same) / fromIntegral (length same)
  where
    chs = map chromosomes $ individuals population
    genesList = transpose $ map genes $ map fst chs ++ map snd chs
    same :: [Bool]
    same = map almostAllTheSame genesList

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev xs = sqrt $ summedElements / count
  where
    avg = average xs
    count = fromIntegral $ length xs
    summedElements = sum (map (\x -> (x - avg) * (x - avg)) xs)

minFitness :: (Int -> Phenotype) -> Int -> Population -> Double
minFitness generationOptimum generationNumber = minFitnessForGeneration (generationOptimum generationNumber)

minFitnessForGeneration :: Phenotype -> Population -> Double
minFitnessForGeneration _       (Population _ []) = nan
minFitnessForGeneration optimum (Population _ is) = minimum $ map (fitness optimum . phenotype) is

percentileFitness :: Double -> (Int -> Phenotype) -> Int -> Population -> Double
percentileFitness _          _                    _              (Population _ []) = nan
percentileFitness percentile generationOptimum generationNumber  (Population _ is) =
    sort (map (fitness (generationOptimum generationNumber) . phenotype) is) !! floor (percentile * fromIntegral (length is))

randomRules :: Int -> Int -> Int -> Int -> RVar [(Schema, Phenotype)]
randomRules baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount = do
    return []

randomOptimum :: RVar Phenotype
randomOptimum = randomPhenotypeFraction 8.0

negativeDominantRule :: (Schema, Phenotype) -> (DominantSchema, Phenotype)
negativeDominantRule (Schema sch, Phenotype ph) = (DominantSchema sch, Phenotype $ map (\d -> -3.0 * d) ph)

positiveDominantRule :: (Schema, Phenotype) -> (DominantSchema, Phenotype)
positiveDominantRule (Schema sch, Phenotype ph) = (DominantSchema sch, Phenotype $ map (\d ->  2.0 * d) ph)

express :: Int -> Int -> Int -> Int -> Int -> Int -> RVar ExpressionStrategy
express baseCount
        pleiotropicRulesCount
        epistaticRulesCount
        complicatedRulesCount
        negativeDominantRulesCount
        positiveDominantRulesCount
        = do

    rules <- randomRules baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount
    -- domRules <- dominantRules baseCount dominantRulesCount
    shuffledRules <- shuffle rules


    let
        domRules = map negativeDominantRule (take negativeDominantRulesCount shuffledRules) ++
                   map positiveDominantRule (take positiveDominantRulesCount $ drop negativeDominantRulesCount shuffledRules)

        matchers = map (matches . fst) rules ++ map (matches . fst) domRules
        changes  = map snd (traceShowId rules) ++ map snd (traceShowId domRules)

    return $ schemaBasedExpression $ zip matchers changes

collapse :: Int -> RVar a -> a
collapse seedValue x = fst $ sampleState x (mkStdGen seedValue)

turbidostatCoefficientsForPopulationSize :: Double -> Int -> Double
turbidostatCoefficientsForPopulationSize accidentDeathProbability' expectedPopulationSize =
      (1 - accidentDeathProbability') / 2.25 / fromIntegral expectedPopulationSize / fromIntegral expectedPopulationSize

optimumCalculation :: Phenotype -> Phenotype -> Int -> Phenotype
optimumCalculation optimum1 optimum2 g =
    if g < optimumChangeGeneration
        then optimum1
        else optimum2

params2rules :: AnalysisParameters -> EvolutionRules
params2rules params =
  let
    baseCount = countOfBases params

    pleiotropicRulesCount = countOfPleiotropicRules params
    epistaticRulesCount = countOfEpistaticRules params
    complicatedRulesCount = countOfComplicatedRules params
    negativeDominantRulesCount = countOfNegativeDominantRules params
    positiveDominantRulesCount = countOfPositiveDominantRules params

    expression' = collapse (seed params) $ express
                                                   baseCount
                                                   pleiotropicRulesCount
                                                   epistaticRulesCount
                                                   complicatedRulesCount
                                                   negativeDominantRulesCount
                                                   positiveDominantRulesCount

    breedingStrategy = if separatedGenerations params
                           then panmictic expression'
                           else panmicticOverlap expression'

    startPopulationSize = populationSize params

    hSelection :: Phenotype -> Selection
    hSelection optimum = hardSelection (fitness optimum) $ hardSelectionThreshold params

    maximumAge = maxAge params

    optimum1 = collapse (seed params + 1) randomOptimum
    optimumC = collapse (seed params + 2) $ randomPhenotypeFraction 8.0
    optimum2 = Phenotype $ zipWithCheck (+) (phenotypeToVector optimum1) (phenotypeToVector optimumC)

    turbidostatCoefficients = turbidostatCoefficientsForPopulationSize accidentDeathProbability (2 * startPopulationSize)

  in
    EvolutionRules { mutation = [ pointMutation expression' ]
                   , breeding = [ breedingStrategy ]
                   , selection = [ hSelection ]
                   , deaths =
                       [ \_ -> turbidostat turbidostatCoefficients accidentDeathProbability
                       , killOld maximumAge
                       ]
                   , expression = expression'
                   , optimumForGeneration = optimumCalculation optimum1 optimum2
                   }

computeSimulation :: AnalysisParameters -> [(String, [(Integer, Double)])]
computeSimulation params =
  let
    rules = params2rules params
    startPopulationSize = populationSize params

    initialPopulation = randomPopulation startPopulationSize (expression rules) $ countOfBases params
    allGenerations = evolution maxSteps rules initialPopulation

    generations :: [Population]
    generations = collapse (seed params + 3) allGenerations

    stats :: (Int -> Population -> Double) -> [(Integer, Double)]
    stats f = zip [0..] (zipWith f [0..] generations)

  in [ ("Avg Fitness", stats $ avgFitness $ optimumForGeneration rules)
     , ("Min Fitness", stats $ minFitness $ optimumForGeneration rules)
     , ("10% percentile Fitness", stats $ percentileFitness 0.1 $ optimumForGeneration rules)
     , ("Ochylka Fitness", stats $ stdDevFitness $ optimumForGeneration rules)
     , ("Population Size", stats $ const $ fromIntegral . length . individuals)
     , ("homozygotness", stats $ homozygotness $ optimumForGeneration rules)
     , ("% of polymorphic locus", stats $ const polymorphism)
     , ("% of locus with allele with more than 90% appearence", stats $ const almostPolymorphism)
     ]