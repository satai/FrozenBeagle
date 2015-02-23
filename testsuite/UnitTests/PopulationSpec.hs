module UnitTests.PopulationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Data.List
import Control.Monad

import Genes
import Population

import UnitTests.GenesSpec(DnaStringOfLen10(DnaStringOfLen10))
import UnitTests.GenesSpec(arbitraryDna)

instance Arbitrary Individual where
	arbitrary = do
		DnaStringOfLen10 d1 <- arbitrary
		DnaStringOfLen10 d2 <- arbitrary
		return $ Individual(d1, d2)
  	
spec :: Spec
spec = parallel $ do
	describe "Population" $ do

		it "individual has two different dna strings" $ 
			property ( \(Individual(dna1, dna2)) -> 
				dna1 /= dna2
			)
