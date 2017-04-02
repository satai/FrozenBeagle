module SimulationConstants ( dimensionCount
                           , zeroPhenotypeVec
                           ) where

dimensionCount :: Int
dimensionCount = 4

zeroPhenotypeVec :: [Double]
zeroPhenotypeVec = replicate dimensionCount 0.0

