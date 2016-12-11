{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.SchemaSpec(spec, Arbitrary, allMatchingSchema) where

import Test.Hspec
import Test.QuickCheck

import Genes
import Schema
import UnitTests.GenesSpec()

instance Arbitrary Schema where
     arbitrary = Schema <$> vector 10

allMatchingSchema :: Schema
allMatchingSchema = Schema $ replicate 10 Nothing

spec :: Spec
spec = parallel $ do
    describe "Schema" $ do

        it "show schema string has the same length as schema + 2" $
            property  ( \(Schema elems) ->
                ((length elems) + 2) == (length $ show $ Schema elems)
            )

        it "show schema string looks like {13*5*}" $
            (show (Schema [Just G1, Just G3, Nothing,  Just G5, Nothing]) `shouldBe` "{13*5*}" )

        it "order of schema is count of specified positions" $
            order (Schema [Just G1, Just G3, Nothing,  Just G5, Nothing]) `shouldBe` 3

        it "schema matches dna if they have same content" $
            property (
                \(DnaString elems) ->
                    matches (Schema $ map Just elems) (DnaString elems)
            )

        it "schema doesn't match dna with different content" $
            (not $ matches (Schema [Just G1, Just G1]) (DnaString [G1, G2]))

        it "schema does match dna with different same same content on specified places" $
            (matches (Schema [Just G2, Nothing]) (DnaString [G2, G4]))
