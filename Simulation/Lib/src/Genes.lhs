This module exports Genes as lists of bases, bases names and crossover and mutation operations.

> module Genes
>          ( DnaString(DnaString)
>          , genes
>          , Alela(..)
>          , crossover
>          , mutate
>          ) where
>

Alelas are the abstract G1..G3

> data Alela = G1 | G2 | G3
>     deriving (Eq, Ord)
>
> instance Show Alela where
>   show G1 = "1"
>   show G2 = "2"
>   show G3 = "3"
>

DnaString is defined by the list of bases.

> newtype DnaString = DnaString {genes :: [Alela]} deriving(Eq)
>

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
> mutate :: Int -> Alela -> DnaString -> DnaString
> mutate mutationPoint newAlela (DnaString dna) =
>     DnaString $ zipWithIndex (\oldAlela i -> if i == mutationPoint then newAlela else oldAlela) dna
>     where zipWithIndex f l = zipWith f l [0..]
