{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.GenesSpec (spec, DnaString, Alela) where

import Test.Hspec
import Test.QuickCheck
import Data.List

import Genes
import ListUtils

instance Arbitrary Alela where
  arbitrary =
       elements [ G1
                , G2
                , G3
                ]

instance Arbitrary DnaString where
     arbitrary = DnaString <$> vector 10

newtype PointInString = PointInString Int deriving Show
instance Arbitrary PointInString  where
     arbitrary = PointInString <$> elements [0..11]

newtype IndexInString = IndexInString Int deriving Show
instance Arbitrary IndexInString  where
     arbitrary = IndexInString <$> elements [0..9]

spec :: Spec
spec = parallel $
    describe "Genes" $ do

        it "show DNA string looks like '[12213]'" $
            show (DnaString [G1, G2, G2, G1, G3]) `shouldBe` "[12213]"

        it "show DNA strings are lexicographicaly ordered" $
            DnaString [G1, G2, G2, G1, G3] `compare` DnaString [G1, G2, G3, G1, G3]
                `shouldBe`
            LT

        it "length of crosovered dna is the same as the mother dnas" $
            property ( \(DnaString dna1) (DnaString dna2)  ->
                length dna1
                    ==
                length (genes $ crossover 4 (DnaString dna1) (DnaString dna2))
            )

        it "beginning of crosovered DNA is from the first mother string" $
            property ( \(PointInString n) (DnaString dna1) (DnaString dna2)  ->
                take n dna1
                    ==
                take n (genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "end of crosovered DNA is from the second mother string" $
            property ( \(PointInString n) (DnaString dna1) (DnaString dna2)  ->
                drop n dna2
                    ==
                drop n (genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "crosovered DNA is of the same length as mother dnas" $
            property ( \(PointInString n) (DnaString dna1) (DnaString dna2)  ->
                length dna1
                    ==
                length (genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "mutated dna has same length as dna before mutation" $
            property ( \(IndexInString n) b (DnaString dna) ->
                length dna
                    ==
                length (genes $ mutate n b (DnaString dna))
            )

        it "mutated dna has new Alela at point of mutation" $
            property ( \(IndexInString n)  b  (DnaString dna ) ->
                b == genes (mutate n b (DnaString dna)) !! n
            )

        it "mutated dna doesn't differ from original one with exception of point of mutation" $
            property ( \(IndexInString n) b (DnaString dna ) ->
                 1 >= length (elemIndices True $ zipWithCheck (/=) dna (genes $ mutate n b (DnaString dna)) )
            )