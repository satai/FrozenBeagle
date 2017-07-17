module Simulation.Internal
    ( computeSimulation
    , AnalysisParameters(..)
    , turbidostatCoefficientsForPopulationSize
    , optimumCalculation
    , randomOptimum
    , collapse
    , randomPopulation
   -- , randomRules
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
import           Control.Exception.Base
import           Data.List hiding (union)
import           Data.MultiSet (MultiSet, fromList, toOccurList, distinctSize, unions, union)

import           Data.Random
import           Data.Random.Distribution.Bernoulli
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
    , ratioOfNegativeDominantRules   :: Double
    , ratioOfPositiveDominantRules   :: Double
    , ratioOfPleiotropicRules        :: Double
    , seed                           :: Int
    } deriving Show

randomPopulation :: Int -> ExpressionStrategy -> Double  -> Double -> Int -> RVar Population
randomPopulation count expressionStrategy ratioOfNegativeDominance probabilityPleiotropic baseCount = Population 0 <$> randomIndividuals count expressionStrategy ratioOfNegativeDominance probabilityPleiotropic baseCount

randomIndividuals :: Int -> ExpressionStrategy -> Double -> Double -> Int -> RVar [Individual]
randomIndividuals count expressionStrategy ratioOfNegativeDominance probabilityPleiotropic baseCount = replicateM count $ randomIndividual ratioOfNegativeDominance probabilityPleiotropic baseCount expressionStrategy

randomIndividual :: Double ->Double -> Int -> ExpressionStrategy -> RVar Individual
randomIndividual ratioOfNegativeDominance probabilityPleiotropic baseCount expressionStrategy = do
    individualsSex <- randomSex
    chs <- randomChromosomes ratioOfNegativeDominance probabilityPleiotropic baseCount
    return $ Individual individualsSex 0 chs $ expressionStrategy individualsSex chs

randomSex :: RVar Sex
randomSex = choice [F, M]

randomChromosomes :: Double -> Double -> Int -> RVar (DnaString, DnaString)
randomChromosomes ratioOfNegativeDominance ratioPleiotropic baseCount = do
    dna1 <- randomDnaString ratioOfNegativeDominance ratioPleiotropic baseCount
    dna2 <- randomDnaString ratioOfNegativeDominance ratioPleiotropic baseCount
    return (dna1, dna2)

randomDnaString :: Double -> Double -> Int -> RVar DnaString
randomDnaString ratioOfNegativeDominance ratioPleiotropic baseCount = DnaString <$> replicateM baseCount (randomInitAllele ratioOfNegativeDominance ratioPleiotropic )

randomInitAllele :: Double -> Double -> RVar Allele
randomInitAllele ratioNegativeDominance ratioPleiotropic  = do
    isZero <- boolBernoulli (0.0 :: Float)

    if isZero
        then return $ Allele zeroPhenotype zeroPhenotype
        else randomAllele ratioNegativeDominance ratioPleiotropic

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

dominantHomozygotness :: (Int -> Phenotype) -> Int -> Population -> Double
dominantHomozygotness _ _ population =
    let
        is = individuals population
        chs = map chromosomes is
        ps = zip (concatMap (genes . fst) chs) (concatMap (genes . snd) chs)
        homos = filter (\p -> effect p /= dominantEffect p) $ map fst $ filter (uncurry (==)) ps
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

aleleCount :: Population -> Double
aleleCount population = fromIntegral $ distinctSize allAllelas
    where
        allAllelas :: MultiSet Allele
        allAllelas = unions $ map (\(c1, c2) -> union (fromList $ genes c1) (fromList $ genes c2)) $ map chromosomes $ individuals population

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

randomRules :: Int -> Int -> RVar [(Schema, Phenotype)]
randomRules baseCount epistaticRulesCount = do
    let _ = assert $ baseCount > 0
        _ = assert $ epistaticRulesCount == 0
    return []

randomOptimum :: RVar Phenotype
randomOptimum = randomPhenotypeFraction optimumSizeCoefficient

express :: Int -> Int -> RVar ExpressionStrategy
express baseCount
        epistaticRulesCount
        = do

    rules <- randomRules baseCount epistaticRulesCount

    let
        matchers = map (matches . fst) rules
        changes  = map snd (traceShowId rules)

    return $ commonExpression $ schemaBasedExpression $ zip matchers changes

collapse :: Int -> RVar a -> a
collapse seedValue x = fst $ sampleState x (mkStdGen seedValue)

turbidostatCoefficientsForPopulationSize :: Double -> Int -> Int -> Double
turbidostatCoefficientsForPopulationSize accidentDeathProbability' maximumAge expectedPopulationSize =
      (4 - 12 * accidentDeathProbability' - (12.0 / fromIntegral maximumAge)) / 27.0 / fromIntegral expectedPopulationSize / fromIntegral expectedPopulationSize

optimumCalculation :: Phenotype -> Phenotype -> Int -> Phenotype
optimumCalculation optimum1 optimum2 g =
    if g < optimumChangeGeneration || g > 2 * optimumChangeGeneration
        then optimum1
        else optimum2

params2rules :: AnalysisParameters -> EvolutionRules
params2rules params =
  let
    baseCount = countOfBases params

    epistaticRulesCount = countOfEpistaticRules params
    negativeDominantRulesRatio = ratioOfNegativeDominantRules params
    pleiotropicRulesRatio = ratioOfPleiotropicRules params
    -- FIXME positiveDominantRulesRatio = ratioOfPositiveDominantRules params

    expression' = collapse (seed params) $ express
                                                   baseCount
                                                   epistaticRulesCount

    breedingStrategy = if separatedGenerations params
                           then panmictic expression'
                           else panmicticOverlap expression'

    startPopulationSize = populationSize params

    hSelection :: Phenotype -> Selection
    hSelection optimum = hardSelection (fitness optimum) $ hardSelectionThreshold params

    maximumAge = maxAge params

    optimum1 = collapse (seed params + 1) randomOptimum
    optimumC = collapse (seed params + 2) $ randomPhenotypeFraction optimumChangeSizeCoefficient
    optimum2 = Phenotype $ zipWithCheck (+) (phenotypeToVector optimum1) (phenotypeToVector optimumC)

    turbidostatCoefficients = turbidostatCoefficientsForPopulationSize accidentDeathProbability maximumAge startPopulationSize

  in
    EvolutionRules { mutation = [ pointMutation negativeDominantRulesRatio pleiotropicRulesRatio]
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

    initialPopulation = randomPopulation startPopulationSize (expression rules) (ratioOfNegativeDominantRules params) (ratioOfPleiotropicRules params) $ countOfBases params
    allGenerations = evolution maxSteps rules initialPopulation

    generations :: [Population]
    generations = collapse (seed params + 3) allGenerations

    stats :: (Int -> Population -> Double) -> [(Integer, Double)]
    stats f = zip [0..] (zipWith f [0..] generations)

  in [ ("Avg Fitness", stats $ avgFitness $ optimumForGeneration rules)
     , ("Min Fitness", stats $ minFitness $ optimumForGeneration rules)
     , ("10% percentile Fitness", stats $ percentileFitness 0.1 $ optimumForGeneration rules)
     , ("Odchylka Fitness", stats $ stdDevFitness $ optimumForGeneration rules)
     , ("Population Size", stats $ const $ fromIntegral . length . individuals)
     , ("homozygotness", stats $ homozygotness $ optimumForGeneration rules)
     , ("% of dominant homozygotes", stats $ dominantHomozygotness $ optimumForGeneration rules)
     , ("% of polymorphic locus", stats $ const polymorphism)
     , ("% of locus with allele with more than 90% appearence", stats $ const almostPolymorphism)
     , ("# different alalas", stats $ const aleleCount)
     ]