module Phenotype(Phenotype(Phenotype), distance, phenotypeToVector) where

data Phenotype = Phenotype [Double] deriving (Eq, Show, Ord)

phenotypeToVector :: Phenotype -> [Double]
phenotypeToVector (Phenotype xs) = xs

distance :: Phenotype -> Phenotype -> Double
distance (Phenotype p1) (Phenotype p2) = sqrt $ sum $ map (^2) $ zipWith (-) p1 p2
