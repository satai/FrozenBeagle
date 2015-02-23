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


spec :: Spec
spec = parallel $ do
	describe "Genes" $ do

		it "length of crosovered dna is the same as the mother dnas" $ 
			property ( \(DnaStringOfLen10 dna1) -> \(DnaStringOfLen10 dna2)  -> 
				(length dna1) == (length $ crossover 4 dna1 dna2)
			)

		it "beginning of crosovered DNA is from the first mother string" $ 
			property ( \(DnaStringOfLen10 dna1) -> \(DnaStringOfLen10 dna2)  -> 
				(take 4 dna1) == (take 4 $ crossover 4 dna1 dna2)
			)

		it "end of crosovered DNA is from the second mother string" $ 
			property ( \(DnaStringOfLen10 dna1) -> \(DnaStringOfLen10 dna2)  -> 
				(drop 4 dna2) == (drop 4 $ crossover 4 dna1 dna2)
			)
