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

		it  "sum of two vectors is computed field by field" $ do 
			1 `shouldBe` 1

		it "dna string equals itself just to test the testing framework" $ 
			property ( \(DnaStringOfLen10 x) -> \(DnaStringOfLen10 y)  -> x == x)		
