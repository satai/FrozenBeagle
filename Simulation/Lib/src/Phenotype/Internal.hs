module Phenotype.Internal
    ( Phenotype (Phenotype)
    , distance
    , phenotypeToVector
    , zeroPhenotype
    , randomPhenotypeFraction
    , randomPhenotypeChange
    , randomPhenotypeChangeWithOneNonzero
    , fitness) where

import ListUtils
import Control.Monad
import Data.Random
import Data.Random.Distribution.Normal

import SimulationConstants

newtype Phenotype = Phenotype [Double] deriving (Eq, Show, Ord)

zeroPhenotype :: Phenotype
zeroPhenotype = Phenotype zeroPhenotypeVec

randomPhenotypeFraction :: Double -> RVar Phenotype
randomPhenotypeFraction d = Phenotype . map (* d) <$> replicateM dimensionCount doubleStdNormal

randomPhenotypeChange :: RVar Phenotype
randomPhenotypeChange = randomPhenotypeFraction 1.0

randomPhenotypeChangeWithOneNonzero :: RVar Phenotype
randomPhenotypeChangeWithOneNonzero = do
        r <- doubleStdNormal
        Phenotype <$> shuffle (r : tail zeroPhenotypeVec)

phenotypeToVector :: Phenotype -> [Double]
phenotypeToVector (Phenotype xs) = xs

distance :: Phenotype -> Phenotype -> Double
distance (Phenotype p1) (Phenotype p2) = euclideanDistance p1 p2
  where
    sqr :: Double -> Double
    sqr x = x * x

    euclideanDistance :: [Double] -> [Double] -> Double
    euclideanDistance a b = sqrt $ sum $ map sqr $ zipWithCheck (-) a b

fitness :: Phenotype -> Phenotype -> Double
fitness optimum individual = exp $ (-0.004) * ((individual `distance` optimum) ** 2)
