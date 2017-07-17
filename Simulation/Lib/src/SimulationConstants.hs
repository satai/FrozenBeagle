module SimulationConstants ( dimensionCount
                           , zeroPhenotypeVec
                           , maxSteps
                           , optimumChangeGeneration
                           , accidentDeathProbability
                           , probabilityAlleleMutation
                           , optimumSizeCoefficient
                           , optimumChangeSizeCoefficient
                           , fitnessDecreaseCoefficient
                           , negativeDominanceScale
                           , negativeDominanceShape
                           , nan
                           ) where

dimensionCount :: Int
dimensionCount = 4

zeroPhenotypeVec :: [Double]
zeroPhenotypeVec = replicate dimensionCount 0.0

maxSteps :: Int
maxSteps = 3 * optimumChangeGeneration

optimumChangeGeneration :: Int
optimumChangeGeneration = 8 * 1024

accidentDeathProbability :: Double
accidentDeathProbability = 0.0

probabilityAlleleMutation :: Double
probabilityAlleleMutation = 0.0002

nan :: Double
nan = 0.0 / 0.0

optimumSizeCoefficient :: Double
optimumSizeCoefficient = 12.0

optimumChangeSizeCoefficient :: Double
optimumChangeSizeCoefficient = 12.0

fitnessDecreaseCoefficient :: Double
fitnessDecreaseCoefficient = -0.004

negativeDominanceScale :: Double
negativeDominanceScale = 1.5

negativeDominanceShape :: Double
negativeDominanceShape = 1.5