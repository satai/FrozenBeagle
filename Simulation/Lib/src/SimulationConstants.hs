module SimulationConstants ( dimensionCount
                           , zeroPhenotypeVec
                           , maxSteps
                           , optimumChangeGeneration
                           , accidentDeathProbability
                           , probabilityIndividualMutation
                           , probabilityBasisMutation
                           , maximumCountOfOffspring
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

probabilityIndividualMutation :: Float
probabilityIndividualMutation = 0.01

probabilityBasisMutation :: Float
probabilityBasisMutation = 0.02

maximumCountOfOffspring :: Int
maximumCountOfOffspring = 6

