{-# LANGUAGE DeriveGeneric #-}

module Phenotype
    ( Phenotype (Phenotype)
    , distance
    , phenotypeToVector
    , fitness) where

import Data.Hashable

newtype Phenotype = Phenotype [Double] deriving (Eq, Show, Ord)

instance Hashable Phenotype where
    hashWithSalt salt (Phenotype p) = hashWithSalt salt p

phenotypeToVector :: Phenotype -> [Double]
phenotypeToVector (Phenotype xs) = xs

distance :: Phenotype -> Phenotype -> Double
distance (Phenotype p1) (Phenotype p2) = euclideanDistance p1 p2
  where
    sqr :: Double -> Double
    sqr x = x * x

    euclideanDistance :: [Double] -> [Double] -> Double
    euclideanDistance a b = sqrt $ sum $ map sqr $ zipWith (-) a b

fitness :: Phenotype -> Phenotype -> Double
fitness optimum individual = log $ 25.0 / (individual `distance` optimum + 0.001)
