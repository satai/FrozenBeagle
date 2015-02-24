module Phenotype(Phenotype(Phenotype), distance) where

data Phenotype = Phenotype [Double] deriving (Eq, Show)

distance :: Phenotype -> Phenotype -> Double
distance (Phenotype p1) (Phenotype p2) = sqrt $ sum $ map (^2) $ zipWith (-) p1 p2