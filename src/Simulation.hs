module Simulation(computeSimulation, AnalysisParameters(AnalysisParameters)) where

import Evolution
import Expression
import Schema
import Genes
import Phenotype
import Population

import Data.Functor
import Data.Random
import Data.Random.Extras
import Data.Random.Distribution.Uniform
import System.Random

data AnalysisParameters = AnalysisParameters {
    multipleRuns :: Bool,
    separatedGenerations :: Bool,
    populationSize :: Int
    }

randomPopulation :: Int -> RVar Population
randomPopulation count = Population <$> randomIndividuals count

randomIndividuals :: Int -> RVar [Individual]
randomIndividuals count = sequence $ take count $ repeat randomIndividual

randomIndividual :: RVar Individual
randomIndividual = do
        gender <- randomGender
        chromosomes <- randomChromosomes
        return $ Individual gender chromosomes

randomGender :: RVar Sex
randomGender = choice [F, M]

randomChromosomes :: RVar (DnaString, DnaString)
randomChromosomes = do
            dna1 <- randomDnaString
            dna2 <- randomDnaString
            return (dna1, dna2)

randomDnaString :: RVar DnaString
randomDnaString = DnaString <$> (sequence $ take 10 $ repeat randomBase)

randomBase :: RVar Basis
randomBase = choice [G,T,C,A]

avgFitness :: Population -> Double
avgFitness (Population is) = average $ map fitness is

average :: [Double] -> Double
average [] = 0.0
average xs = (sum xs) / fromIntegral (length xs)

minFitness :: Population -> Double
minFitness (Population individuals) = minimum $ map fitness individuals

fitness :: Individual -> Double
fitness = fitness' (Phenotype [1.0, 1.0, 0, 0])  --fixme

fitness' :: Phenotype -> Individual -> Double
fitness' optimum individual = 1.0 / (express individual `distance` optimum + 0.001)

randomRules :: RVar [(Schema, Phenotype)]
randomRules =  sequence $ take 100 $ repeat randomRule

randomRule = do
    schema <- randomSchema
    phenotype <- randomPhenotype
    return (schema, phenotype)

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

express = schemaBasedExpression $ fst $ sampleState (randomRules) (mkStdGen 0)

colapse :: RVar a -> a
colapse x = fst $ sampleState x (mkStdGen 0)

computeSimulation :: AnalysisParameters -> [(String, [(Int, Double)])]
computeSimulation params =
    let
        initialPopulation = randomPopulation $ populationSize params
        rules = EvolutionRules {
                                    mutation = return,
                                    breeding = return,
                                    selection = hardSelection fitness 0.1
                                }   --FIXME
        generations = take 100 $ evolution rules initialPopulation
        allGenerations = map colapse generations
        stats f = zip [0..] (map f allGenerations)
    in
        [("Avg Fitness", stats avgFitness), ("Min Fitness", stats minFitness)]
