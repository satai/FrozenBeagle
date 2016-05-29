module Phenotype(Phenotype(Phenotype), distance, phenotypeToVector) where

data Phenotype = Phenotype [Double] deriving (Eq, Show, Ord)

phenotypeToVector :: Phenotype -> [Double]
phenotypeToVector (Phenotype xs) = xs

distance :: Phenotype -> Phenotype -> Double
distance (Phenotype p1) (Phenotype p2) = euclideanDistance p1 p2
  where
    sqr :: Double -> Double
    sqr x = x * x

    euclideanDistance :: [Double] -> [Double] -> Double
    euclideanDistance a b = sqrt $ sum $ map sqr $ zipWith (-) a b
