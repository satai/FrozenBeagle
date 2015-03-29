module Simulation(computeSimulation, AnalysisParameters(AnalysisParameters)) where

import Data.Random

import Evolution

data AnalysisParameters = AnalysisParameters {
    multipleRuns :: Bool,
    separatedGenerations :: Bool,
    populationSize :: Int
    }

signal :: [Int] -> [(Int,Double)]
signal xs = [ (round x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- map fromIntegral xs ]

signal2 xs = [ (round x,(cos (x*3.14159/45) + 1) / 2 * (cos (x*3.14159/5))) | x <-  map fromIntegral xs ]

computeSimulation :: AnalysisParameters -> [(String, [(Int, Double)])]
computeSimulation params = 
    [("t1", signal [0,7..400]), ("t2", signal2 [0,7..400])]