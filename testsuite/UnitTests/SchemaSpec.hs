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

        it "show schema string looks like {GT*T*}" $
            (show (Schema [Just G, Just T, Nothing,  Just T, Nothing]) `shouldBe` "{GT*T*}" )

        it "order of schema is count of specified positions" $
            order (Schema [Just G, Just T, Nothing,  Just T, Nothing]) `shouldBe` 3

        it "schema matches dna if they have same content" $
            property (
                \(DnaString elems) ->
                    matches (Schema $ map Just elems) (DnaString elems)
            )

        it "schema doesn't match dna with different content" $
            (not $ matches (Schema [Just T, Just T]) (DnaString [T, A]))

        it "schema does match dna with different same same content on specified places" $
            (matches (Schema [Just T, Nothing]) (DnaString [T, A]))
