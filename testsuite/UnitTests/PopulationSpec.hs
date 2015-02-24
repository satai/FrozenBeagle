module UnitTests.PopulationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Data.List
import Control.Monad

import Genes
import Population

import UnitTests.GenesSpec(DnaString)

instance Arbitrary Individual where
    arbitrary = do
        d1 <- arbitrary
        d2 <- arbitrary
        return $ Individual(d1, d2)
      
spec :: Spec
spec = parallel $ do
    describe "Population" $ do

        it "individual has two different dna strings" $ 
            property ( \(Individual(dna1, dna2)) -> 
                dna1 /= dna2    --if these are equal, there is something wrong in our random generation, there is 1Mi combinations
            )
