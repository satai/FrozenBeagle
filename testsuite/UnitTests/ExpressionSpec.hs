module UnitTests.ExpressionSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor

import Genes
import Population
import Phenotype
import Schema
import Expression

import UnitTests.PopulationSpec()
import UnitTests.PhenotypeSpec()
import UnitTests.SchemaSpec(allMatchingSchema)

spec :: Spec
spec = parallel $ do
    describe "Schema based Expression" $ do

        it "Is zero vector for no schema" $
            property (\sex chromosomes ->
                schemaBasedExpression [] sex chromosomes `shouldBe` Phenotype [0, 0, 0, 0]
            )

        it "Is zero vector when no matching schema" $
            property (\phenotypeChange ->
                schemaBasedExpression [(Schema $ replicate 10 $ Just A, phenotypeChange)] M (DnaString $ replicate 10 T, DnaString $ replicate 10 G) `shouldBe` Phenotype [0, 0, 0, 0]
            )

        it "Moves phenotype if matches" $
            property (\phenotypeChange sex chromosomes ->
                schemaBasedExpression [(allMatchingSchema, phenotypeChange)] sex chromosomes `shouldBe` phenotypeChange
            )

        it "Moves phenotype for each matching schema" $
            property (\sex chromosomes ->
                schemaBasedExpression [(allMatchingSchema, Phenotype [1, 0, 0, 1]), (allMatchingSchema, Phenotype [0, 1, 0, 1])] sex chromosomes `shouldBe` Phenotype [1, 1, 0, 2]
            )
