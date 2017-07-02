{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.SchemaSpec(spec, Arbitrary, allMatchingSchema) where

import Test.Hspec
import Test.QuickCheck

import Control.Exception.Base
import Debug.Trace

import Genes
import Phenotype
import Schema
import UnitTests.GenesSpec()

instance Arbitrary Schema where
     arbitrary = Schema <$> vector 8

allMatchingSchema :: Schema
allMatchingSchema = Schema $ replicate 8 Nothing

spec :: Spec
spec = parallel $
    describe "Schema" $ do

        it "show schema string contains as many * as it's length - order" $
            property  ( \(Schema elems) ->
                length elems - order (Schema elems) == length (filter (== '*') (show (Schema elems)))
            )

        it "schema equals itself" $
            property ( \schema ->
                (schema :: Schema) == schema
            )

        it "different schemas don't equal" $
             property  $ Schema [Just $ Allele (Phenotype [1]) (Phenotype [2])] /= Schema [Just $ Allele (Phenotype [3]) (Phenotype [2])]

        it "schema equals an other schema with iff they have the same content" $
            property  ( \elems1 elems2 ->
                (elems1 == elems2) == (Schema elems1 == Schema elems2)
            )

        it "show schema string looks like {13*5*}"
            (show (Schema [ Just $ Allele (Phenotype [1]) (Phenotype [2])
                          , Just $ Allele (Phenotype [1]) (Phenotype [2])
                          , Nothing
                          , Just $ Allele (Phenotype [3]) (Phenotype [1])
                          , Nothing]
                  ) `shouldBe` "<{(1.0)|(2.0)}{(1.0)|(2.0)}*{(3.0)|(1.0)}*>" )

        it "order of schema is count of specified positions" $
            order (Schema [ Just (Allele (Phenotype []) (Phenotype []))
                          , Just (Allele (Phenotype []) (Phenotype []))
                          , Nothing
                          , Just (Allele (Phenotype []) (Phenotype []))
                          , Nothing
                          ])
               `shouldBe`
            3

        it "schema matches dna if they have same content" $
            property (
                \(DnaString elems) (DnaString otherElems) ->
                    matches (Schema $ map Just elems) (DnaString elems) (DnaString otherElems) &&
                    matches (Schema $ map Just elems) (DnaString otherElems) (DnaString elems)
            )

--         it "schema doesn't match dna with different content" $
--             not (matches (Schema [Just G1, Just G1]) (DnaString [G1, G2]) (DnaString [G2, G2])) &&
--             not (matches (Schema [Just G1, Just G1]) (DnaString [G2, G2]) (DnaString [G1, G2]))

        it "schema does match dna with different same same content on specified places" $
            property ( \a a' ->
                matches (Schema [Nothing, Just a, Nothing, Just a'])
                                      (DnaString [a, a, a', a'])
                                      (DnaString [a, a, a , a ])
                   `shouldBe`
                True
            )

        it "match fails in runtime when schema list longer then first dna" $
            property ( \a ->
                evaluate (traceShowId $ matches (Schema [Nothing, Nothing, Nothing])
                                                (DnaString [a, a])
                                                (DnaString [a, a, a]) )
                   `shouldThrow`
                errorCall "Incompatible Schema and DNA"
            )

        it "match fails in runtime when schema list longer then second dna" $
            property ( \a ->
                evaluate (matches (Schema [Nothing, Nothing, Nothing])
                                                (DnaString [a, a, a])
                                                (DnaString [a, a]) )
                   `shouldThrow`
                errorCall "Incompatible Schema and DNA"
            )

        it "match fails in runtime when schema list shorter then first dna" $
            property ( \a ->
                evaluate (traceShowId $ matches (Schema [Nothing, Just a])
                                                (DnaString [a, a, a])
                                                (DnaString [a, a]) )
                   `shouldThrow`
                errorCall "Incompatible Schema and DNA"
            )

        it "match fails in runtime when schema list shorter then second dna" $
            property ( \a ->
                evaluate (matches (Schema [Nothing, Just a])
                                                (DnaString [a, a])
                                                (DnaString [a, a, a]) )
                   `shouldThrow`
                errorCall "Incompatible Schema and DNA"
            )