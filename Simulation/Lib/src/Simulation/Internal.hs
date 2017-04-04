module Simulation.Internal
    ( computeSimulation
    , AnalysisParameters(..)
    , pleiotropicRules
    , randomPleiotropicRule
    , turbidostatCoefficientsForPopulationSize
    , optimumCalculation
    , randomPhenotypeChange
    , randomOptimum
    , collapse
    , randomPopulation
    , randomRules
    ) where

import           SimulationConstants
import           Evolution
import           Expression
import           Genes
import           Individual
import           Phenotype
import           Population
import           Schema

import           Control.Monad
import           Data.List
import           Data.MultiSet                    (fromList, toOccurList)

import           Data.Random
import           Data.Random.Distribution.Normal
import           Data.Random.Distribution.Uniform
import           Data.Random.Extras hiding (shuffle)
import           System.Random

import Debug.Trace

data AnalysisParameters = AnalysisParameters
    { separatedGenerations    :: Bool
    , hardSelectionThreshold  :: Double
    , populationSize          :: Int
    , optimumChange           :: [(Double, Double, Double)]
    , maxAge                  :: Int
    , countOfBases            :: Int
    , countOfPleiotropicRules :: Int
    , countOfEpistaticRules   :: Int
    , countOfComplicatedRules :: Int
    , countOfDominantRules    :: Int
    , seed                    :: Int
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
randomDnaString baseCount = DnaString <$> replicateM baseCount randomBase

randomBase :: RVar Basis
randomBase = choice [ G1
                    , G2
                    , G3
                    ]

avgFitness :: (Int -> Phenotype) -> Int -> Population -> Double
avgFitness generationOptimum generationNumber = avgFitnessForGeneration (generationOptimum generationNumber)

avgFitnessForGeneration :: Phenotype -> Population -> Double
avgFitnessForGeneration optimum (Population _ is)  = average $ map (fitness optimum . phenotype) is

stdDevFitness :: (Int -> Phenotype) -> Int -> Population -> Double
stdDevFitness generationOptimum generationNumber = stdDevFitnessForGeneration (generationOptimum generationNumber)

stdDevFitnessForGeneration :: Phenotype -> Population -> Double
stdDevFitnessForGeneration optimum (Population _ is)  = stdDev $ map (fitness optimum . phenotype) is

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

almostAllTheSame :: (Ord a) => [a] -> Bool
almostAllTheSame xs = (0.90 :: Double) * fromIntegral (length xs)  >= fromIntegral (maximum $ map snd $ toOccurList $ fromList xs)

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
minFitnessForGeneration _       (Population _ []) = 1.0 / 0.0
minFitnessForGeneration optimum (Population _ is) = minimum $ map (fitness optimum . phenotype) is

percentileFitness :: Double -> (Int -> Phenotype) -> Int -> Population -> Double
percentileFitness _          _                    _              (Population _ []) = 1.0 / 0.0
percentileFitness percentile generationOptimum generationNumber  (Population _ is) =
    sort (map (fitness (generationOptimum generationNumber) . phenotype) is) !! floor (percentile * fromIntegral (length is))

randomRules :: Int -> Int -> Int -> Int -> RVar [(Schema, Phenotype)]
randomRules baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount = do
    br <- basicRules baseCount
    er <- epistaticRules baseCount epistaticRulesCount
    pr <- pleiotropicRules baseCount pleiotropicRulesCount
    cr <- complicatedRules baseCount complicatedRulesCount
    return (br ++ pr ++ er ++ cr)

epistaticRules :: Int -> Int -> RVar [(Schema, Phenotype)]
epistaticRules baseCount countOfRules = replicateM countOfRules (randomEpistaticRule baseCount)

-- FIXME use it
-- dominantRules :: Int -> Int -> RVar [(DominantSchema, Phenotype)]
-- dominantRules baseCount countOfRules = replicateM countOfRules (randomDominantRule baseCount)

pleiotropicRules :: Int -> Int -> RVar [(Schema, Phenotype)]
pleiotropicRules baseCount countOfRules = replicateM countOfRules (randomPleiotropicRule baseCount)

randomPleiotropicRule :: Int -> RVar (Schema, Phenotype)
randomPleiotropicRule baseCount = do
    position <- integralUniform 0 (baseCount - 1)
    basis <- randomBase
    dimChange <- randomPhenotypeChange

    let
        schema = replicate position Nothing ++ [Just basis] ++ replicate (baseCount - position - 1) Nothing

    return (Schema schema, dimChange)

basicRules :: Int -> RVar [(Schema, Phenotype)]
basicRules baseCount = concat <$> mapM (simpleRulesForPosition baseCount) [0..(baseCount - 1)]

complicatedRules :: Int -> Int -> RVar [(Schema, Phenotype)]
complicatedRules baseCount countOfRules = replicateM countOfRules (randomComplicatedRule baseCount)

simpleRulesForPosition :: Int -> Int -> RVar [(Schema, Phenotype)]
simpleRulesForPosition baseCount p = do
    dimension <- integralUniform 0 (dimensionCount - 1)

    g1Change <- doubleStdNormal
    g2Change <- doubleStdNormal
    g3Change <- doubleStdNormal

    let g1DimChange = replicate dimension 0.0 ++ [g1Change] ++ replicate (dimensionCount - dimension - 1) 0.0
    let g2DimChange = replicate dimension 0.0 ++ [g2Change] ++ replicate (dimensionCount - dimension - 1) 0.0
    let g3DimChange = replicate dimension 0.0 ++ [g3Change] ++ replicate (dimensionCount - dimension - 1) 0.0

    let schema1 = replicate p Nothing ++ [Just G1] ++ replicate (baseCount - p - 1) Nothing
    let schema2 = replicate p Nothing ++ [Just G2] ++ replicate (baseCount - p - 1) Nothing
    let schema3 = replicate p Nothing ++ [Just G3] ++ replicate (baseCount - p - 1) Nothing

    return [ (Schema schema1, Phenotype g1DimChange)
           , (Schema schema2, Phenotype g2DimChange)
           , (Schema schema3, Phenotype g3DimChange)
           ]

randomEpistaticRule :: Int -> RVar (Schema, Phenotype)
randomEpistaticRule baseCount = do
    schema <- randomSchema baseCount
    dimension <- integralUniform 0 (dimensionCount - 1)
    gChange <- doubleStdNormal

    let gDimChange = replicate dimension 0.0 ++ [gChange] ++ replicate (dimensionCount - dimension - 1) 0.0

    return (schema, Phenotype gDimChange)

randomComplicatedRule :: Int -> RVar (Schema, Phenotype)
randomComplicatedRule baseCount = do
    schema <- randomSchema baseCount
    p <- randomPhenotypeChange
    return (schema, p)

--FIXME use it
-- randomDominantRule :: Int -> RVar (DominantSchema, Phenotype)
-- randomDominantRule baseCount = do
--     schema <- randomDominantSchema baseCount
--     g1Change <- doubleStdNormal
--
--     let
--         dimChange = g1Change : replicate (dimensionCount - 1) 0.0
--
--     p <- shuffle dimChange
--
--     return (schema, Phenotype p)

randomSchema :: Int -> RVar Schema
randomSchema baseCount = do
    b1 <- randomBase
    b2 <- randomBase
    b3 <- randomBase
    let
       a = take baseCount $ [Just b1, Just b2, Just b3] ++ repeat Nothing
    Schema <$> shuffle a

-- FIXME use it
-- randomDominantSchema :: Int -> RVar DominantSchema
-- randomDominantSchema baseCount = do
--     b1 <- randomBase
--     let
--        a = Just b1 : replicate (baseCount - 1) Nothing
--     DominantSchema <$> shuffle a

randomPhenotypeFraction :: Double -> RVar Phenotype
randomPhenotypeFraction d = Phenotype . map (* d) <$> replicateM dimensionCount doubleStdNormal

randomPhenotypeChange :: RVar Phenotype
randomPhenotypeChange = randomPhenotypeFraction 1.0

randomOptimum :: RVar Phenotype
randomOptimum = randomPhenotypeFraction 4.0

negativeDominantRule :: (Schema, Phenotype) -> (DominantSchema, Phenotype)
negativeDominantRule (Schema sch, Phenotype ph) = (DominantSchema sch, Phenotype $ map (\d -> -2.0 * d) ph)

express :: Int -> Int -> Int -> Int -> Int -> RVar ExpressionStrategy
express baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount dominantRulesCount = do
    rules <- randomRules baseCount pleiotropicRulesCount epistaticRulesCount complicatedRulesCount
    -- domRules <- dominantRules baseCount dominantRulesCount
    domRules <- take dominantRulesCount . map negativeDominantRule <$> shuffle rules

    let
        matchers = map (matches . fst) rules ++ map (matches . fst) domRules
        changes = map snd (traceShowId rules) ++ map snd (traceShowId domRules)

    return $ schemaBasedExpression $ zip matchers changes

collapse :: Int -> RVar a -> a
collapse seedValue x = fst $ sampleState x (mkStdGen seedValue)

turbidostatCoefficientsForPopulationSize :: Double -> Int -> Double
turbidostatCoefficientsForPopulationSize accidentDeathProbability' expectedPopulationSize =
      (0.5 - accidentDeathProbability') / fromIntegral expectedPopulationSize / fromIntegral expectedPopulationSize

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
    dominantRulesCount = countOfDominantRules params

    expression' = collapse (seed params) $ express
                                                   baseCount
                                                   pleiotropicRulesCount
                                                   epistaticRulesCount
                                                   complicatedRulesCount
                                                   dominantRulesCount

    breedingStrategy = if separatedGenerations params
                           then panmictic expression'
                           else panmicticOverlap expression'

    startPopulationSize = populationSize params

    hSelection :: Phenotype -> Selection
    hSelection optimum = hardSelection (fitness optimum) $ hardSelectionThreshold params

    maximumAge = maxAge params

    optimum1 = collapse (seed params + 1) randomOptimum
    optimumC = collapse (seed params + 2) $ randomPhenotypeFraction 4.0
    optimum2 = Phenotype $ zipWith (+) (phenotypeToVector optimum1) (phenotypeToVector optimumC)

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
     , ("% of polymorphic locus", stats $ const polymorphism)
     , ("% of locus with alela with more than 90% appearence", stats $ const almostPolymorphism)
     ]       -- fixme
