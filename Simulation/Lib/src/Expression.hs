module Expression
    ( ExpressionStrategy
    , schemaBasedExpression
    ) where

import Genes
import Schema
import Phenotype
import Sex

type ExpressionStrategy = Sex -> (DnaString, DnaString) -> Phenotype

schemaBasedExpression :: [(Schema, Phenotype)] -> ExpressionStrategy
schemaBasedExpression xs _ chromosomes = Phenotype $ foldl sumVec [0, 0, 0, 0] $ map phenotypeToVector phenotypeChanges
  where
    (chromosome1, chromosome2) = chromosomes
    sumVec = zipWith (+)
    match schema = matches schema chromosome1 chromosome2
    matchingPairs = filter (match . fst) xs
    phenotypeChanges :: [Phenotype]
    phenotypeChanges = map snd matchingPairs
