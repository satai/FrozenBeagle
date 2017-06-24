This module exports Genes as lists of bases, bases names and crossover and mutation operations.

> module Genes
>          ( DnaString(DnaString)
>          , genes
>          , Allele(..)
>          , crossover
>          , mutate
>          , randomAllele
>          ) where
>
> import Data.Random
> import Data.Random.Distribution.Bernoulli
> import Data.Random.Distribution.Pareto
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

> randomDominantEffect :: Phenotype -> Double -> RVar Phenotype
> randomDominantEffect effect' negativeDominantRatio = do
>    isNegativeDominant <- boolBernoulli negativeDominantRatio
>    effectSize <- pareto 1.0 2.0
>    if isNegativeDominant
>    then return $ Phenotype $ map ((-1.0 * effectSize) *) $ phenotypeToVector effect'
>    else return effect'

> randomAllele :: Double -> RVar Allele
> randomAllele negativeDominanceRatio = do
>     effect' <- randomPhenotypeChangeWithOneNonzero --FIXME
>     dominantEffect' <- randomDominantEffect effect' negativeDominanceRatio
>     return $ Allele effect' dominantEffect'

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
