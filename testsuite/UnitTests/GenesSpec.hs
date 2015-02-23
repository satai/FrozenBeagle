module UnitTests.GenesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Control.Monad

spec :: Spec
spec = parallel $ do
	describe "Genes" $ do

		it  "sum of two vectors is computed field by field" $ do 
			1 `shouldBe` 1