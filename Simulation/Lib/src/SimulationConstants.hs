module SimulationConstants ( dimensionCount
                           , zeroPhenotypeVec
                           , maxSteps
                           , optimumChangeGeneration
                           , accidentDeathProbability
                           , probabilityBasisMutation
                           , nan
                           ) where

dimensionCount :: Int
dimensionCount = 4

zeroPhenotypeVec :: [Double]
zeroPhenotypeVec = replicate dimensionCount 0.0

maxSteps :: Int
maxSteps = 6000

optimumChangeGeneration :: Int
optimumChangeGeneration = maxSteps `div` 2

accidentDeathProbability :: Double
accidentDeathProbability = 0.0

probabilityBasisMutation :: Double
probabilityBasisMutation = 0.0001

nan :: Double
nan = 0.0 / 0.0
