module Phenotype.Internal
    ( Phenotype (Phenotype)
    , distance
    , phenotypeToVector
    , fitness) where

import ListUtils

newtype Phenotype = Phenotype [Double] deriving (Eq, Show, Ord)

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
fitness optimum individual = 4.0 * (exp $ (-0.010) * ((individual `distance` optimum) ** 2))
