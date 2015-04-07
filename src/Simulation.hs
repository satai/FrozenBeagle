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
import Debug.Trace

data AnalysisParameters = AnalysisParameters {
    multipleRuns :: Bool,
    separatedGenerations :: Bool,
    populationSize :: Int
    }

signal :: [Int] -> [(Int,Double)]
signal xs = [ (round x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- map fromIntegral xs ]
randomPopulation :: Int -> RVar Population
randomPopulation count = Population <$> randomIndividuals count

signal2 xs = [ (round x,(cos (x*3.14159/45) + 1) / 2 * (cos (x*3.14159/5))) | x <-  map fromIntegral xs ]
randomIndividuals :: Int -> RVar [Individual]
randomIndividuals count = sequence $ take count $ repeat randomIndividual

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



computeSimulation :: AnalysisParameters -> [(String, [(Int, Double)])]
computeSimulation params = 
    [("t1", signal [0,7..400]), ("t2", signal2 [0,7..400])]