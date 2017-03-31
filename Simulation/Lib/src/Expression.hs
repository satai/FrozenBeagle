module Expression
    ( ExpressionStrategy
    , schemaBasedExpression
    ) where

import Genes
import Phenotype
import Sex

type ExpressionStrategy = Sex -> (DnaString, DnaString) -> Phenotype

schemaBasedExpression :: [(DnaString -> DnaString -> Bool, Phenotype)] -> ExpressionStrategy
schemaBasedExpression xs _ chromosomes = Phenotype $ foldl sumVec [0, 0, 0, 0] $ map phenotypeToVector phenotypeChanges
  where
    (chromosome1, chromosome2) = chromosomes
    sumVec = zipWith (+)
    matchingPairs = filter ((\i -> i chromosome1 chromosome2) . fst) xs
    phenotypeChanges :: [Phenotype]
    phenotypeChanges = map snd matchingPairs
