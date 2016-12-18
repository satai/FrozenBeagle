module Simulation(computeSimulation, AnalysisParameters(AnalysisParameters)) where

import Evolution
import Expression
import Schema
import Genes
import Phenotype
import Population

import Data.Random
import Data.Random.Extras
import Data.Random.Distribution.Uniform
import System.Random

data AnalysisParameters = AnalysisParameters {
    separatedGenerations :: Bool,
    hardSelectionTreshold :: Double,
    populationSize :: Int,
    optimumChange :: [(Double, Double, Double)]
    }

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

average :: [Double] -> Double
average [] = 0.0
average xs = sum xs / fromIntegral (length xs)

minFitness :: Phenotype -> Population -> Double
minFitness optimum (Population _ is) = minimum $ (largestDouble :) $ map (fitness optimum . phenotype) is
    where largestDouble = 1.7976931348623157e308

fitness :: Phenotype -> Phenotype -> Double
fitness optimum individual = 1.0 / (individual `distance` optimum + 0.001)

randomRules :: RVar [(Schema, Phenotype)]
randomRules =  sequence $ take 100 $ repeat randomRule

randomRule :: RVar (Schema, Phenotype)
randomRule = do
    schema <- randomSchema
    p <- randomPhenotype
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
        a1 <- doubleStdUniform
        a2 <- doubleStdUniform
        a3 <- doubleStdUniform
        a4 <- doubleStdUniform
        return $ Phenotype [a1, a2, a3, a4]

express :: ExpressionStrategy
express = schemaBasedExpression $ fst $ sampleState randomRules (mkStdGen 0)

colapse :: RVar a -> a
colapse x = fst $ sampleState x (mkStdGen 0)

params2rules :: AnalysisParameters -> EvolutionRules
params2rules params = --FIXME
    let breedingStrategy = if separatedGenerations params then panmictic express else panmicticOverlap express
        startPopulationSize = populationSize params

        hSelection :: Phenotype -> Selection
        hSelection optimum = hardSelection (fitness optimum) $ hardSelectionTreshold params
    in
        EvolutionRules {
                           mutation = pointMutation express,
                           breeding = breedingStrategy,
                           selection = hSelection,
                           deaths = \g -> turbidostat 0.0000001 0.1 --fixme
                      }

maxSteps :: Int
maxSteps = 500

computeSimulation :: AnalysisParameters -> [(String, [(Integer, Double)])]
computeSimulation params =
    let rules = params2rules params
        startPopulationSize = populationSize params
        initialPopulation = randomPopulation startPopulationSize
        allGenerations = evolution maxSteps rules initialPopulation
        generations = colapse allGenerations
        stats f = zip [0..] (map f generations)
    in
        [("Avg Fitness", stats $ avgFitness $ Phenotype [1.0, 0.0, 0.0, 0.0] ), ("Min Fitness", stats $ minFitness $ Phenotype [1.0, 0.0, 0.0, 0.0]), ("Population Size", stats (fromIntegral . length . individuals))]       -- fixme
