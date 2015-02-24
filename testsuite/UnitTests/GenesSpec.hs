module UnitTests.GenesSpec (spec, DnaString, Basis) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Data.List

import Genes

instance Arbitrary Basis where
  arbitrary = do 
       elements [G, A, T, C]

instance Arbitrary DnaString where
     arbitrary = DnaString <$> vector 10

newtype PointInString = PointInString Int deriving Show
instance Arbitrary PointInString  where
     arbitrary = do
         PointInString <$> elements [0..11]

newtype IndexInString = IndexInString Int deriving Show
instance Arbitrary IndexInString  where
     arbitrary = do
         IndexInString <$> elements [0..9]         

spec :: Spec
spec = parallel $ do
    describe "Genes" $ do

        it "show DNA string looks like '[GATTACA]'" $ 
            (show (DnaString [G, A, T, T, A, C, A]) `shouldBe` "[GATTACA]" )

        it "length of crosovered dna is the same as the mother dnas" $ 
            property ( \(DnaString dna1) -> \(DnaString dna2)  -> 
                (length $ dna1) == 
                    (length $ genes $ crossover 4 (DnaString dna1) (DnaString dna2))
            )

        it "beginning of crosovered DNA is from the first mother string" $ 
            property ( \(PointInString n) -> \(DnaString dna1) -> \(DnaString dna2)  -> 
                (take n dna1) == 
                    (take n $ genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "end of crosovered DNA is from the second mother string" $ 
            property ( \(PointInString n) -> \(DnaString dna1) -> \(DnaString dna2)  -> 
                (drop n dna2) == 
                    (drop n $ genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "mutated dna has same length as dna before mutation" $ 
            property ( \(IndexInString n) -> \b -> \(DnaString dna) -> 
                (length dna) == 
                    (length $ genes $ mutate n b (DnaString dna))
            )

        it "mutated dna has new basis at point of mutation" $ 
            property ( \(IndexInString n) -> \b -> \(DnaString dna ) -> 
                b == 
                    (genes $ mutate n b (DnaString dna)) !! n
            )

        it "mutated dna doesn't differ from original one with exception of point of mutation" $ 
            property ( \(IndexInString n) -> \b -> \(DnaString dna ) -> 
                 1 >= (length $ elemIndices True $ zipWith (/=) dna (genes $ mutate n b (DnaString dna)) )
            )

