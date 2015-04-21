module Expression(ExpressionStrategy, schemaBasedExpression) where

import Genes
import Schema
import Phenotype
import Population

type ExpressionStrategy = Sex -> (DnaString, DnaString) -> Phenotype

schemaBasedExpression :: [(Schema, Phenotype)] -> ExpressionStrategy
schemaBasedExpression xs _ (chromosome1, chromosome2) = Phenotype $ foldl sumVec [0, 0, 0, 0] $ map phenotypeToVector phenotypeChanges
        where
            sumVec :: [Double] -> [Double] -> [Double]
            sumVec v1 v2 = zipWith (+) v1 v2
            match schema = matches schema chromosome1 || matches schema chromosome2
            matchingPairs = filter (match . fst) xs
            phenotypeChanges :: [Phenotype]
            phenotypeChanges = map snd matchingPairs
