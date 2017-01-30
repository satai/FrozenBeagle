module Simulation(computeSimulation, AnalysisParameters(..), turbidostatCoefiecientsForPopulationSize) where

import Evolution
import Expression
import Schema
import Genes
import Phenotype
import Population
import Individual

import Data.MultiSet(toOccurList, fromList)
import Data.List
import Data.Random
import Data.Random.Extras
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal
import System.Random

import Debug.Trace

data AnalysisParameters = AnalysisParameters {
    separatedGenerations :: Bool,
    hardSelectionTreshold :: Double,
    populationSize :: Int,
    optimumChange :: [(Double, Double, Double)],
    maxAge :: Int
    } deriving Show

randomPopulation :: Int -> RVar Population
randomPopulation count = Population 0 <$> randomIndividuals count

randomIndividuals :: Int -> RVar [Individual]
randomIndividuals count = sequence $ replicate count randomIndividual

randomIndividual :: RVar Individual
randomIndividual = do
        gender <- randomGender
        chs <- randomChromosomes
        return $ Individual gender 0 chs $ express gender chs

randomGender :: RVar Sex
randomGender = choice [F, M]

randomChromosomes :: RVar (DnaString, DnaString)
randomChromosomes = do
            dna1 <- randomDnaString
            dna2 <- randomDnaString
            return (dna1, dna2)

randomDnaString :: RVar DnaString
randomDnaString = DnaString <$> sequence (replicate 15 randomBase)

randomBase :: RVar Basis
randomBase = choice [G1, G2, G3, G4, G5]

avgFitness :: Phenotype -> Population -> Double
avgFitness optimum (Population _ is)  = average $ map (fitness optimum . phenotype) is

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

almostAllTheSame :: (Eq a, Ord a) => [a] -> Bool
almostAllTheSame xs = 0.90 * fromIntegral (length xs)  > fromIntegral (maximum $ map snd $ toOccurList $ fromList xs)

polymorphism :: Population -> Double
polymorphism population = 1.0 - (fromIntegral $ length $ filter id same) / (fromIntegral $ length same)
    where
      chs = map chromosomes $ individuals population
      genesList = transpose $ map genes $ map fst chs ++ map snd chs
      same :: [Bool]
      same = map allTheSame genesList

almostPolymorphism :: Population -> Double
almostPolymorphism population = 1.0 - (fromIntegral $ length $ filter id same) / (fromIntegral $ length same)
    where
      chs = map chromosomes $ individuals population
      genesList = transpose $ map genes $ map fst chs ++ map snd chs
      same :: [Bool]
      same = map almostAllTheSame genesList


average :: [Double] -> Double
average [] = 0.0
average xs = sum xs / fromIntegral (length xs)

minFitness :: Phenotype -> Population -> Double
minFitness optimum (Population _ is) = minimum $ (largestDouble :) $ map (fitness optimum . phenotype) is
    where largestDouble = 1.7976931348623157e308


randomRules :: RVar [(Schema, Phenotype)]
randomRules = do
  br <- basicRules
  pr <- pleiotropicRules
  return (br ++ pr)

pleiotropicRules = sequence $ take 20 $ repeat randomRule

basicRules = concat <$> sequence (map simpleRulesForPosition [0..14])

simpleRulesForPosition :: Int -> RVar [(Schema, Phenotype)]
simpleRulesForPosition p = do
    dimension <- integralUniform 0 3
    g1Change <- doubleStdNormal
    g2Change <- doubleStdNormal
    g3Change <- doubleStdNormal
    g4Change <- doubleStdNormal
    g5Change <- doubleStdNormal

    let g1DimChange = replicate dimension 0.0 ++ [g1Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g2DimChange = replicate dimension 0.0 ++ [g2Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g3DimChange = replicate dimension 0.0 ++ [g3Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g4DimChange = replicate dimension 0.0 ++ [g4Change / 8.0] ++ replicate (4 - dimension - 1) 0.0
    let g5DimChange = replicate dimension 0.0 ++ [g5Change / 8.0] ++ replicate (4 - dimension - 1) 0.0

    let schema1 = replicate p Nothing ++ [Just G1] ++ replicate (15 - p - 1) Nothing
    let schema2 = replicate p Nothing ++ [Just G2] ++ replicate (15 - p - 1) Nothing
    let schema3 = replicate p Nothing ++ [Just G3] ++ replicate (15 - p - 1) Nothing
    let schema4 = replicate p Nothing ++ [Just G4] ++ replicate (15 - p - 1) Nothing
    let schema5 = replicate p Nothing ++ [Just G5] ++ replicate (15 - p - 1) Nothing

    return [
        (Schema schema1, Phenotype g1DimChange),
        (Schema schema2, Phenotype g2DimChange),
        (Schema schema3, Phenotype g3DimChange),
        (Schema schema4, Phenotype g4DimChange)
      ]


randomRule :: RVar (Schema, Phenotype)
randomRule = do
    schema <- randomSchema
    p <- randomPhenotypeChange
    return (schema, p)

randomSchema :: RVar Schema
randomSchema = Schema <$> sequence elems
        where
            elems :: [RVar (Maybe Basis)]
            elems =  take 15 $ repeat randomBaseOrNot -- fixme

randomBaseOrNot :: RVar (Maybe Basis)
randomBaseOrNot = choice $ [Just G1, Just G2, Just G3, Just G4] ++ replicate 20 Nothing  -- fixme

randomPhenotype :: RVar Phenotype
randomPhenotype = do
        a1 <- doubleStdNormal
        dims <- Data.Random.shuffle [a1, 0.0, 0.0, 0.0]
        return $ Phenotype dims

randomPhenotypeChange :: RVar Phenotype
randomPhenotypeChange = do
        a1 <- doubleStdUniform
        a2 <- doubleStdUniform
        a3 <- doubleStdUniform
        a4 <- doubleStdUniform
        return $ Phenotype [a1, a2, a3, a4]

express :: ExpressionStrategy
express = schemaBasedExpression $ traceShowId $ fst $ sampleState randomRules (mkStdGen 0)

colapse :: RVar a -> a
colapse x = fst $ sampleState x (mkStdGen 0)

turbidostatCoefiecientsForPopulationSize :: Double -> Int -> Double
turbidostatCoefiecientsForPopulationSize accidentDeathProbability expectedPopulationSize =
      (0.5 - accidentDeathProbability) / fromIntegral expectedPopulationSize / fromIntegral expectedPopulationSize

params2rules :: AnalysisParameters -> EvolutionRules
params2rules params =
    let breedingStrategy = if separatedGenerations params then panmictic express else panmicticOverlap express
        startPopulationSize = populationSize params

        hSelection :: Phenotype -> Selection
        hSelection optimum = hardSelection (fitness optimum) $ hardSelectionTreshold params

        maximumAge = maxAge params

        accidentDeathProbability = 0.05
    in
        EvolutionRules {
                           mutation = [ pointMutation express ],
                           breeding = [ breedingStrategy ],
                           selection = [ hSelection ],
                           deaths = [
                                \g -> turbidostat (turbidostatCoefiecientsForPopulationSize accidentDeathProbability (2 * startPopulationSize)) accidentDeathProbability,
                                \g -> killOld (maxAge params) g
                           ]
                      }

maxSteps :: Int
maxSteps = 1500

computeSimulation :: AnalysisParameters -> [(String, [(Integer, Double)])]
computeSimulation params =
    let rules = params2rules params
        startPopulationSize = populationSize params
        initialPopulation = randomPopulation startPopulationSize
        allGenerations = evolution maxSteps rules initialPopulation
        generations = colapse allGenerations
        stats f = zip [0..] (map f generations)
    in
        [("Avg Fitness", stats $ avgFitness $ Phenotype [1.0, 0.0, 0.0, 0.0] ), ("Min Fitness", stats $ minFitness $ Phenotype [1.0, 0.0, 0.0, 0.0]), ("Population Size", stats (fromIntegral . length . individuals)), ("% of polymorphic locus", stats polymorphism), ("% of locus with alela with more than 90% appearence", stats almostPolymorphism)]       -- fixme
