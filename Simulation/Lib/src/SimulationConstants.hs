module SimulationConstants ( dimensionCount
                           , zeroPhenotypeVec
                           , maxSteps
                           , optimumChangeGeneration
                           , accidentDeathProbability
                           , probabilityAlelaMutation
                           , nan
                           ) where

dimensionCount :: Int
dimensionCount = 4

zeroPhenotypeVec :: [Double]
zeroPhenotypeVec = replicate dimensionCount 0.0

maxSteps :: Int
maxSteps = 4096

optimumChangeGeneration :: Int
optimumChangeGeneration = maxSteps `div` 2

accidentDeathProbability :: Double
accidentDeathProbability = 0.0

probabilityAlelaMutation :: Double
probabilityAlelaMutation = 0.0001

nan :: Double
nan = 0.0 / 0.0
