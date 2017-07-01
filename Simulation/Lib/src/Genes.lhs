This module exports Genes as lists of bases, bases names and crossover and mutation operations.

> module Genes
>          ( DnaString(DnaString)
>          , genes
>          , Allele(..)
>          , crossover
>          , mutate
>          , randomAllele
>          , randomDominantEffect
>          ) where
>
> import Data.List
> import Data.Random
> import Data.Random.Distribution.Bernoulli
> import Data.Random.Distribution.Pareto
>
> import Phenotype
> import SimulationConstants

Alleles are FIXME

> data Allele = Allele
>                { effect :: Phenotype
>                , dominantEffect :: Phenotype
>                }
>     deriving (Eq, Ord)
>
> instance Show Allele where
>     show (Allele ef def) = "{" ++ show ef ++ "|" ++ show def ++ "}"


DnaString is defined by the list of bases.

> newtype DnaString = DnaString {genes :: [Allele]} deriving(Eq)
>

> randomDominantEffect :: Phenotype -> Double -> RVar Phenotype
> randomDominantEffect effect' negativeDominantRatio = do
>    isNegativeDominant <- boolBernoulli negativeDominantRatio
>    effectSize <- pareto negativeDominanceScale negativeDominanceShape
>    if isNegativeDominant
>    then return $ Phenotype $ map ((-1.0 * effectSize) *) $ phenotypeToVector effect'
>    else return effect'

> randomAllele :: Double -> Double -> RVar Allele
> randomAllele negativeDominanceRatio pleiotropicRatio = do
>     isPleiotropic <- boolBernoulli pleiotropicRatio
>     effect' <- if isPleiotropic
>                then randomPhenotypeFraction (1.0 / sqrt (fromIntegral dimensionCount))
>                else randomPhenotypeChangeWithOneNonzero
>     dominantEffect' <- randomDominantEffect effect' negativeDominanceRatio
>     return $ Allele effect' dominantEffect'

DnaStrings have a common lexicographic ordering. It's handy for constructing data structure but not used any other way.

> instance Ord DnaString where
>     (DnaString d1) `compare` (DnaString d2) = d1 `compare` d2
>
> instance Show DnaString where
>     show (DnaString elems) = "[" ++ intercalate ", " (map show elems) ++ "]"
>
> crossover :: Int -> DnaString -> DnaString -> DnaString
> crossover crossoverPoint (DnaString dna1) (DnaString dna2) = DnaString $ take crossoverPoint dna1 ++ drop crossoverPoint dna2
>
> mutate :: Int -> Allele -> DnaString -> DnaString
> mutate mutationPoint newAllele (DnaString dna) =
>     DnaString $ zipWithIndex (\oldAllele i -> if i == mutationPoint then newAllele else oldAllele) dna
>     where zipWithIndex f l = zipWith f l [0..]
