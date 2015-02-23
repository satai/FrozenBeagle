module Genes(DnaString, Basis(G,A,T,C)) where

data Basis = G | A | T | C 
	deriving (Eq, Show)

type DnaString = [Basis]

