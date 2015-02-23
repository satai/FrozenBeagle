module Genes(DnaString, Basis(G,A,T,C), crossover) where

data Basis = G | A | T | C 
	deriving (Eq, Show)

type DnaString = [Basis]

crossover :: Int -> DnaString -> DnaString -> DnaString
crossover crossoverPoint dna1 dna2 =  (take crossoverPoint dna1) ++ (drop crossoverPoint dna2)