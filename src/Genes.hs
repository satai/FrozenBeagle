module Genes(DnaString, Basis(G,A,T,C), crossover, mutate) where

data Basis = G | A | T | C 
	deriving (Eq, Show)

type DnaString = [Basis]

crossover :: Int -> DnaString -> DnaString -> DnaString
crossover crossoverPoint dna1 dna2 = (take crossoverPoint dna1) ++ (drop crossoverPoint dna2)

mutate :: Int -> Basis -> DnaString -> DnaString
mutate mutationPoint newBasis dna = 
	zipWith (\oldBasis -> \i -> if (i == mutationPoint) then newBasis else oldBasis) dna [0..]