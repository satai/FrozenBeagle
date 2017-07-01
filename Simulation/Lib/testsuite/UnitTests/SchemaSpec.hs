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
spec = parallel $ do
    describe "Schema" $ do

        it "show schema string has the same length as schema + 2" $
            property  ( \(Schema elems) ->
                (2 + length elems) == length (show $ Schema elems)
            )

        it "schema equals itself" $
            property ( \schema ->
                (schema :: Schema) == schema
            )

--         it "different schemas don't equal" $
--             property  $ Schema [Just G2] /= Schema [Just G1]

        it "schema equals an other schema with iff they have the same content" $
            property  ( \elems1 elems2 ->
                (elems1 == elems2) == (Schema elems1 == Schema elems2)
            )
--
--         it "show schema string looks like {13*5*}"
--             (show (Schema [Just G1, Just G2, Nothing,  Just G1, Nothing]) `shouldBe` "{12*1*}" )

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
--
--         it "schema does match dna with different same same content on specified places" $
--             matches (Schema [Nothing, Just G2, Nothing]) (DnaString [G1, G2, G1]) (DnaString [G2, G1, G2]) &&
--             matches (Schema [Nothing, Just G2, Nothing]) (DnaString [G1, G1, G1]) (DnaString [G2, G2, G2])
-- --
-- --         it "match fails in runtime when schema list longer then first dna" $
--             evaluate (traceShowId $ matches (Schema [Just G3, Just G2, Nothing]) (DnaString [G1, G2]) (DnaString [G2, G1, G2]) )
--                `shouldThrow`
--             errorCall "Incompatible Schema and DNA"

--         it "match fails in runtime when schema list longer then second dna" $
--             evaluate (traceShowId $ matches (Schema [Nothing, Nothing, Nothing]) (DnaString [G1, G2, G1]) (DnaString [G2, G1]) )
--                `shouldThrow`
--             errorCall "Incompatible Schema and DNA"
--
--         it "match fails in runtime when schema list longer then both dnas" $
--             evaluate (traceShowId $ matches (Schema [Nothing, Nothing, Nothing]) (DnaString [G1, G2]) (DnaString [G2, G1]) )
--                `shouldThrow`
--             errorCall "Incompatible Schema and DNA"
--
--         it "match fails in runtime when schema list shorter then first dna" $
--             evaluate (traceShowId $ matches (Schema [Nothing, Just G2]) (DnaString [G1, G2, G1]) (DnaString [G2, G1]) )
--                `shouldThrow`
--             errorCall "Incompatible Schema and DNA"
--
--         it "match fails in runtime when schema list shorter then second dna" $
--             evaluate (traceShowId $ matches (Schema [Nothing, Nothing]) (DnaString [G1, G2]) (DnaString [G2, G1, G3]) )
--                `shouldThrow`
--             errorCall "Incompatible Schema and DNA"
--
--         it "match fails in runtime when schema list shorter then both dnas" $
--             evaluate (traceShowId $ matches (Schema [Nothing, Nothing]) (DnaString [G1, G2, G3]) (DnaString [G2, G1, G3]) )
--                `shouldThrow`
--             errorCall "Incompatible Schema and DNA"
