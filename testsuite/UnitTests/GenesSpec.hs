module UnitTests.GenesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Control.Monad

import Genes

instance Arbitrary Basis where
  arbitrary = elements [G, A, T, C]

newtype DnaStringOfLen10  = DnaStringOfLen10 DnaString deriving Show

instance Arbitrary DnaStringOfLen10  where
 	arbitrary = do 
 		DnaStringOfLen10 <$> vector 10

newtype PointInString = PointInString Int deriving Show
instance Arbitrary PointInString  where
 	arbitrary = do
 		PointInString <$> elements [0..11]

newtype PointInsideString = PointInsideString Int deriving Show
instance Arbitrary PointInsideString  where
 	arbitrary = do
 		PointInsideString <$> elements [1..10] 		

spec :: Spec
spec = parallel $ do
	describe "Genes" $ do

		it "length of crosovered dna is the same as the mother dnas" $ 
			property ( \(DnaStringOfLen10 dna1) -> \(DnaStringOfLen10 dna2)  -> 
				(length dna1) == (length $ crossover 4 dna1 dna2)
			)

		it "beginning of crosovered DNA is from the first mother string" $ 
			property ( \(PointInString n) -> \(DnaStringOfLen10 dna1) -> \(DnaStringOfLen10 dna2)  -> 
				(take n dna1) == (take n $ crossover n dna1 dna2)
			)

		it "end of crosovered DNA is from the second mother string" $ 
			property ( \(PointInString n) -> \(DnaStringOfLen10 dna1) -> \(DnaStringOfLen10 dna2)  -> 
				(drop n dna2) == (drop n $ crossover n dna1 dna2)
			)
