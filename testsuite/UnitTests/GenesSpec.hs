module UnitTests.GenesSpec (spec, ArbitraryDna(ArbitraryDna)) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Data.List

import Genes

instance Arbitrary Basis where
  arbitrary = do 
  	 elements [G, A, T, C]

data ArbitraryDna = ArbitraryDna DnaString deriving Show

instance Arbitrary ArbitraryDna where
 	arbitrary = ArbitraryDna <$> vector 10

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

		it "length of crosovered dna is the same as the mother dnas" $ 
			property ( \(ArbitraryDna dna1) -> \(ArbitraryDna dna2)  -> 
				(length dna1) == (length $ crossover 4 dna1 dna2)
			)

		it "beginning of crosovered DNA is from the first mother string" $ 
			property ( \(PointInString n) -> \(ArbitraryDna dna1) -> \(ArbitraryDna dna2)  -> 
				(take n dna1) == (take n $ crossover n dna1 dna2)
			)

		it "end of crosovered DNA is from the second mother string" $ 
			property ( \(PointInString n) -> \(ArbitraryDna dna1) -> \(ArbitraryDna dna2)  -> 
				(drop n dna2) == (drop n $ crossover n dna1 dna2)
			)

		it "mutated dna has same length as dna before mutation" $ 
			property ( \(IndexInString n) -> \b -> \(ArbitraryDna dna) -> 
				(length dna) == (length $ mutate n b dna)
			)

		it "mutated dna has new basis at point of mutation" $ 
			property ( \(IndexInString n) -> \b -> \(ArbitraryDna dna ) -> 
				b == mutate n b dna !! n
			)

		it "mutated dna doesn't differ from original one with exception of point of mutation" $ 
			property ( \(IndexInString n) -> \b -> \(ArbitraryDna dna ) -> 
				 1 >= (length $ elemIndices True $ zipWith (/=) dna (mutate n b dna) )
			)

