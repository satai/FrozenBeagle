module Expression
    ( ExpressionStrategy
    , schemaBasedExpression
    ) where

import Genes
import ListUtils
import Phenotype
import Sex
import SimulationConstants

type ExpressionStrategy = Sex -> (DnaString, DnaString) -> Phenotype

schemaBasedExpression :: [(DnaString -> DnaString -> Bool, Phenotype)] -> ExpressionStrategy
schemaBasedExpression xs _ chromosomes = Phenotype $ foldl sumVec zeroPhenotypeVec $ map phenotypeToVector phenotypeChanges
  where
    (chromosome1, chromosome2) = chromosomes
    sumVec = zipWithCheck (+)
    matchingPairs = filter ((\i -> i chromosome1 chromosome2) . fst) xs
    phenotypeChanges :: [Phenotype]
    phenotypeChanges = map snd matchingPairs
