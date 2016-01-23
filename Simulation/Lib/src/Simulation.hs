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
randomPopulation count = Population <$> randomIndividuals count

randomIndividuals :: Int -> RVar [Individual]
randomIndividuals count = sequence $ replicate count randomIndividual

randomIndividual :: RVar Individual
randomIndividual = do
        gender <- randomGender
        chs <- randomChromosomes
        return $ Individual gender chs $ express gender chs

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
randomBase = choice [G,T,C,A]

avgFitness :: Population -> Double
avgFitness (Population is) = average $ map (fitness . phenotype) is

average :: [Double] -> Double
average [] = 0.0
average xs = sum xs / fromIntegral (length xs)

minFitness :: Population -> Double
minFitness (Population is) = minimum $ (largestDouble :) $ map (fitness . phenotype) is
    where largestDouble = 1.7976931348623157e308

fitness :: Phenotype -> Double
fitness = fitness' (Phenotype [1.0, 1.0, 0, 0])  --fixme

fitness' :: Phenotype -> Phenotype -> Double
fitness' optimum individual = 1.0 / (individual `distance` optimum + 0.001)

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
randomBaseOrNot = choice [Just A, Just G, Just T, Just C, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] -- fixme

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

computeSimulation :: AnalysisParameters -> [(String, [(Integer, Double)])]
computeSimulation params =
    let startPopulationSize = populationSize params
        initialPopulation = randomPopulation startPopulationSize
        breedingStrategy = if separatedGenerations params then panmictic express else panmicticOverlap express
        rules = EvolutionRules {
                                    mutation = return, --FIXME
                                    breeding = breedingStrategy,
                                    selection = fittest startPopulationSize fitness -- FIXME add hardSelection fitness 0.1
                                }   --FIXME
        generations = take 100 $ evolution rules initialPopulation
        allGenerations = map colapse generations
        stats f = zip [0..] (map f allGenerations)
    in
        [("Avg Fitness", stats avgFitness), ("Min Fitness", stats minFitness), ("Population Size", stats (fromIntegral . length . individuals))]
