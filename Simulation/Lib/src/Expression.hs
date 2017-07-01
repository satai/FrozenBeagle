module Expression
    ( ExpressionStrategy
    , schemaBasedExpression
    , commonExpression
    , simpleExpression
    ) where

import Genes
import ListUtils
import Phenotype
import Sex
import SimulationConstants

type ExpressionStrategy = Sex -> (DnaString, DnaString) -> Phenotype

sumVec :: [Double] -> [Double] -> [Double]
sumVec = zipWithCheck (+)

commonExpression :: ExpressionStrategy -> ExpressionStrategy
commonExpression = combinedExpression simpleExpression

combinedExpression :: ExpressionStrategy -> ExpressionStrategy -> ExpressionStrategy
combinedExpression ex1 ex2 s dnas = Phenotype $ zipWith (+) (phenotypeToVector $ ex1 s dnas) (phenotypeToVector $ ex2 s dnas)

simpleExpression :: ExpressionStrategy
simpleExpression _ (DnaString chromosome1, DnaString chromosome2) = Phenotype $ foldl sumVec zeroPhenotypeVec $ zipWith addeneum chromosome1 chromosome2
  where
    addeneum :: Allele -> Allele -> [Double]
    addeneum allele1 allele2 =
            if allele1 == allele2
            then phenotypeToVector $ dominantEffect allele1
            else zipWith (+) (phenotypeToVector $ effect allele1) (phenotypeToVector $ effect allele2)


schemaBasedExpression :: [(DnaString -> DnaString -> Bool, Phenotype)] -> ExpressionStrategy
schemaBasedExpression xs _ chromosomes = Phenotype $ foldl sumVec zeroPhenotypeVec $ map phenotypeToVector phenotypeChanges
  where
    (chromosome1, chromosome2) = chromosomes
    matchingPairs = filter ((\i -> i chromosome1 chromosome2) . fst) xs
    phenotypeChanges :: [Phenotype]
    phenotypeChanges = map snd matchingPairs
