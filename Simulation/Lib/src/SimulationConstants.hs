module SimulationConstants ( dimensionCount
                           , zeroPhenotypeVec
                           , maxSteps
                           , optimumChangeGeneration
                           , accidentDeathProbability
                           , probabilityIndividualMutation
                           , probabilityBasisMutation
                           , maximumCountOfOffspring
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

probabilityIndividualMutation :: Double
probabilityIndividualMutation = 0.01

probabilityBasisMutation :: Double
probabilityBasisMutation = 0.02

maximumCountOfOffspring :: Int
maximumCountOfOffspring = 6

nan :: Double
nan = 0.0 / 0.0
