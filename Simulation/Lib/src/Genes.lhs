> {-# LANGUAGE DeriveGeneric #-}

This module exports Genes as lists of bases, bases names and crossover and mutation operations.

> module Genes(DnaString(DnaString), genes, Basis(G,A,T,C), crossover, mutate) where
>
> import Data.Hashable
> import GHC.Generics (Generic)

Bases are the common ones: cytosine, guanine, adenine and thymine,

> data Basis = G | A | T | C
>     deriving (Eq, Show, Ord, Generic)
>
> instance Hashable Basis

DnaString is defined by the list of bases.

> data DnaString = DnaString {genes :: [Basis]} deriving(Eq, Generic)
>
> instance Hashable DnaString

DnaStrings have a common lexiografic ordering. It's handy for constructing data structure but not used any other way.

> instance Ord DnaString where
>     (DnaString d1) `compare` (DnaString d2) = d1 `compare` d2
>
> instance Show DnaString where
>     show (DnaString elems) = "[" ++ map (head . show) elems ++ "]"
>
> crossover :: Int -> DnaString -> DnaString -> DnaString
> crossover crossoverPoint (DnaString dna1) (DnaString dna2) = DnaString $ take crossoverPoint dna1 ++ drop crossoverPoint dna2
>
> mutate :: Int -> Basis -> DnaString -> DnaString
> mutate mutationPoint newBasis (DnaString dna) =
>     DnaString $ zipWithIndex (\oldBasis i -> if i == mutationPoint then newBasis else oldBasis) dna
>     where zipWithIndex f l = zipWith f l [0..]
