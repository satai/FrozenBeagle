module UnitTests.ExpressionSpec(spec) where

import Test.Hspec
import Test.QuickCheck

import Genes
import Population.Internal
import Phenotype
import SimulationConstants
import Schema
import Expression

import UnitTests.PopulationSpec()
import UnitTests.PhenotypeSpec()
import UnitTests.SchemaSpec(allMatchingSchema)

spec :: Spec
spec = parallel $ do
    describe "Schema based Expression" $ do

        it "Is zero vector for no schema" $
            property (\ s chs ->
                schemaBasedExpression [] s chs `shouldBe` (Phenotype $ replicate dimensionCount 0.0)
            )

        it "Is zero vector when no matching schema" $
            property (\ phenotypeChange ->
                schemaBasedExpression [ (matches $ Schema $ replicate 8 $ Just $ Allele (Phenotype [3.0])
                                                                                         (Phenotype [3.0]),
                                         phenotypeChange) ]
                                      M
                                      ( DnaString $ replicate 8 $ Allele (Phenotype [1.0]) (Phenotype [1.0]),
                                        DnaString $ replicate 8 $ Allele (Phenotype [2.0]) (Phenotype [2.0])
                                      )
                    `shouldBe`
                Phenotype [0, 0, 0, 0]
            )

        it "Moves phenotype if matches" $
            property (\ phenotypeChange s chs ->
                schemaBasedExpression [(matches allMatchingSchema, phenotypeChange)] s chs `shouldBe` phenotypeChange
            )

        it "Moves phenotype for each matching schema" $
            property (\ s chs ->
                schemaBasedExpression [(matches allMatchingSchema, Phenotype [1, 0, 0, 1]),
                                       (matches allMatchingSchema, Phenotype [0, 1, 0, 1])
                                      ]
                                      s
                                      chs
                    `shouldBe`
                Phenotype [1, 1, 0, 2]
            )

    describe "Simple Expression" $ do
        it "Is sum of parts for non-dominant allelas" $
            property (\ s ->
                simpleExpression s
                    ( DnaString $ replicate 8 $ Allele (Phenotype [1.0,  0.0, 0.0, 0.0]) zeroPhenotype,
                      DnaString $ replicate 8 $ Allele (Phenotype [0.0, -1.0, 0.0, 0.0]) zeroPhenotype
                    )
                    `shouldBe`
                Phenotype [8.0, -8.0, 0.0, 0.0]
            )

        it "Is sum of dominant parts for dominant allelas" $
            property (\ s ->
                simpleExpression s
                    ( DnaString $ replicate 8 $ Allele zeroPhenotype (Phenotype [1.0, 0.0, 0.0, 0.0]),
                      DnaString $ replicate 8 $ Allele zeroPhenotype (Phenotype [1.0, 0.0, 0.0, 0.0])
                    )
                    `shouldBe`
                Phenotype [8.0, 0.0, 0.0, 0.0]
            )

    describe "Common Expression" $
        it "Is sum of simple and schema based" $
            property (\ s ->
                commonExpression
                    ( schemaBasedExpression [ (matches allMatchingSchema, Phenotype [0,  0, 0, 1])
                                            , (matches allMatchingSchema, Phenotype [0, -1, 0, 0])
                                            ])
                    s
                    ( DnaString $ replicate 8 $ Allele (Phenotype [1.0,  0.0, 0.0, 0.0]) zeroPhenotype,
                      DnaString $ replicate 8 $ Allele (Phenotype [0.0, -1.0, 0.0, 0.0]) zeroPhenotype
                    )
                    `shouldBe`
                Phenotype [8.0, -9.0, 0.0, 1.0]
            )