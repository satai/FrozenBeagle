{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.SchemaSpec(spec, Arbitrary, allMatchingSchema) where

import Test.Hspec
import Test.QuickCheck

import Genes
import Schema
import UnitTests.GenesSpec()

instance Arbitrary Schema where
     arbitrary = Schema <$> vector 10


instance Arbitrary DominantSchema where
     arbitrary = DominantSchema <$> vector 10

allMatchingSchema :: Schema
allMatchingSchema = Schema $ replicate 10 Nothing

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

        it "different schemas don't equal" $
            property  $ Schema [Just G2] /= Schema [Just G1]

        it "schema equals an other schema with iff they have the same content" $
            property  ( \elems1 elems2 ->
                (elems1 == elems2) == (Schema elems1 == Schema elems2)
            )

        it "show schema string looks like {13*5*}"
            (show (Schema [Just G1, Just G2, Nothing,  Just G1, Nothing]) `shouldBe` "{12*1*}" )

        it "order of schema is count of specified positions" $
            order (Schema [Just G1, Just G2, Nothing,  Just G2, Nothing]) `shouldBe` 3

        it "schema matches dna if they have same content" $
            property (
                \(DnaString elems) (DnaString otherElems) ->
                    matches (Schema $ map Just elems) (DnaString elems) (DnaString otherElems) &&
                    matches (Schema $ map Just elems) (DnaString otherElems) (DnaString elems)
            )

        it "schema doesn't match dna with different content" $
            not (matches (Schema [Just G1, Just G1]) (DnaString [G1, G2]) (DnaString [G2, G2])) &&
            not (matches (Schema [Just G1, Just G1]) (DnaString [G2, G2]) (DnaString [G1, G2]))

        it "schema does match dna with different same same content on specified places" $
            matches (Schema [Nothing, Just G2, Nothing]) (DnaString [G1, G2, G1]) (DnaString [G2, G1, G2]) &&
            matches (Schema [Nothing, Just G2, Nothing]) (DnaString [G1, G1, G1]) (DnaString [G2, G2, G2])

    describe "DominantSchema" $ do

        it "show dominant schema string has the same length as schema + 2" $
            property  ( \(DominantSchema elems) ->
                (2 + length elems) == length (show $ DominantSchema elems)
            )

        it "dominant schema equals itself" $
            property  ( \schema ->
                (schema :: DominantSchema) == schema
            )

        it "different domainant schemas don't equal" $
            property  $ DominantSchema [Just G2] /= DominantSchema [Just G1]

        it "dominant schema equals an other dominant schema with iff they have the same content" $
            property  ( \elems1 elems2 ->
                (elems1 == elems2) == (DominantSchema elems1 == DominantSchema elems2)
            )

        it "show dominant schema string looks like (13*5*)"
            (show (DominantSchema [Just G1, Just G2, Nothing,  Just G1, Nothing]) `shouldBe` "(12*1*)" )

        it "order of dominant schema is count of specified positions" $
            order (DominantSchema [Just G1, Just G2, Nothing,  Just G2, Nothing]) `shouldBe` 3

        it "dominant schema matches dna if they have same content" $
            property (
                \(DnaString elems) ->
                    matches (DominantSchema $ map Just elems) (DnaString elems) (DnaString elems)
            )

        it "schema doesn't match dna with different content" $
            not (matches (DominantSchema [Just G1, Just G1]) (DnaString [G1, G1]) (DnaString [G2, G2])) &&
            not (matches (DominantSchema [Just G1, Just G1]) (DnaString [G2, G2]) (DnaString [G1, G1]))

        it "schema does match dna with different same same content on specified places"
            (matches (DominantSchema [Nothing, Just G2, Nothing]) (DnaString [G1, G2, G1]) (DnaString [G2, G2, G2]))
