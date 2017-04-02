module UnitTests.ExpressionSpec(spec) where

import Test.Hspec
import Test.QuickCheck

import Genes
import Population
import Phenotype
import SimulationConstants
import Schema
import Expression

import UnitTests.PopulationSpec()
import UnitTests.PhenotypeSpec()
import UnitTests.SchemaSpec(allMatchingSchema)

spec :: Spec
spec = parallel $
    describe "Schema based Expression" $ do

        it "Is zero vector for no schema" $
            property (\ s chs ->
                schemaBasedExpression [] s chs `shouldBe` (Phenotype $ replicate dimensionCount 0.0)
            )

        it "Is zero vector when no matching schema" $
            property (\ phenotypeChange ->
                schemaBasedExpression [(matches $ Schema $ replicate 10 $ Just G1, phenotypeChange)] M (DnaString $ replicate 10 G2, DnaString $ replicate 10 G2) `shouldBe` Phenotype [0, 0, 0, 0]
            )

        it "Moves phenotype if matches" $
            property (\ phenotypeChange s chs ->
                schemaBasedExpression [(matches allMatchingSchema, phenotypeChange)] s chs `shouldBe` phenotypeChange
            )

        it "Moves phenotype for each matching schema" $
            property (\ s chs ->
                schemaBasedExpression [(matches allMatchingSchema, Phenotype [1, 0, 0, 1]), (matches allMatchingSchema, Phenotype [0, 1, 0, 1])] s chs `shouldBe` Phenotype [1, 1, 0, 2]
            )
