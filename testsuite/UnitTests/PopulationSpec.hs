module UnitTests.PopulationSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import Data.Functor
import Data.List

import Population

import UnitTests.GenesSpec(DnaString)

instance Arbitrary Individual where
    arbitrary = do
        d1 <- arbitrary
        d2 <- arbitrary
        return $ Individual(d1, d2)
      
instance Arbitrary Population where
    arbitrary = Population <$> Set.fromList <$> Test.QuickCheck.arbitrary


spec :: Spec
spec = parallel $ do
    describe "Population" $ do

        it "individual has two different dna strings" $
            property ( \(Individual(dna1, dna2)) ->
                dna1 /= dna2    --if these are equal, there is something wrong in our random generation, there is 1Mi combinations
            )

    describe "allSurvive selection" $ do
        it "doesn't change the population" $
            property ( \population -> 
                population == allSurvive population
            )

