module Genes(DnaString(DnaString), genes, Basis(G,A,T,C), crossover, mutate) where

data Basis = G | A | T | C
    deriving (Eq, Show, Ord)

data DnaString = DnaString {genes :: [Basis]} deriving(Eq)

instance Ord DnaString where
    (DnaString d1) `compare` (DnaString d2) = d1 `compare` d2

instance Show DnaString where
    show (DnaString elems) = "[" ++ (map (head . show) elems) ++ "]"

crossover :: Int -> DnaString -> DnaString -> DnaString
crossover crossoverPoint (DnaString dna1) (DnaString dna2) = DnaString $ (take crossoverPoint dna1) ++ (drop crossoverPoint dna2)

mutate :: Int -> Basis -> DnaString -> DnaString
mutate mutationPoint newBasis (DnaString dna) =
    DnaString $ zipWithIndex (\oldBasis -> \i -> if (i == mutationPoint) then newBasis else oldBasis) dna
    where zipWithIndex f l = zipWith f l [0..]