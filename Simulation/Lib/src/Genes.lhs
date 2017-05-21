This module exports Genes as lists of bases, bases names and crossover and mutation operations.

> module Genes
>          ( DnaString(DnaString)
>          , genes
>          , Allele(..)
>          , crossover
>          , mutate
>          ) where
>
> import Phenotype

Alleles are FIXME

> data Allele = Allele
>                { effect :: Phenotype
>                , dominantEffect :: Phenotype
>                }
>     deriving (Eq, Ord, Show)


DnaString is defined by the list of bases.

> newtype DnaString = DnaString {genes :: [Allele]} deriving(Eq)
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
> mutate :: Int -> Allele -> DnaString -> DnaString
> mutate mutationPoint newAllele (DnaString dna) =
>     DnaString $ zipWithIndex (\oldAllele i -> if i == mutationPoint then newAllele else oldAllele) dna
>     where zipWithIndex f l = zipWith f l [0..]
