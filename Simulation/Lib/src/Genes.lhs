> {-# LANGUAGE DeriveGeneric #-}

This module exports Genes as lists of bases, bases names and crossover and mutation operations.

> module Genes
>          ( DnaString(DnaString)
>          , genes
>          , Basis(..)
>          , crossover
>          , mutate
>          ) where
>
> import Data.Hashable
> import GHC.Generics (Generic)

Bases are the abstract G1..G5

> data Basis = G1 | G2 | G3 | G4 | G5
>     deriving (Eq, Ord, Generic, Enum, Bounded)
>
> instance Show Basis where
>   show G1 = "1"
>   show G2 = "2"
>   show G3 = "3"
>   show G4 = "4"
>   show G5 = "5"
>
> instance Hashable Basis

DnaString is defined by the list of bases.

> newtype DnaString = DnaString {genes :: [Basis]} deriving(Eq, Generic)
>
> instance Hashable DnaString

DnaStrings have a common lexicographic ordering. It's handy for constructing data structure but not used any other way.

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
