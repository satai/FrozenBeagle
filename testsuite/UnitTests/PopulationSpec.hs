module UnitTests.PopulationSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import System.Random
import Data.Functor
import Data.List
import Data.Random
import Data.Random.Extras
import Data.Random.Source.Std

import Population

import UnitTests.GenesSpec(DnaString)

instance Arbitrary Individual where
    arbitrary = do
        sex <- elements [M, F]
        d1 <- arbitrary
        d2 <- arbitrary
        return $ Individual sex (d1, d2)
      
instance Arbitrary Population where
    arbitrary = Population <$> arbitrary

spec :: Spec
spec = parallel $ do
    describe "Population" $ do

        it "individual has two different dna strings" $
            property ( \(Individual sex (dna1, dna2)) ->
                dna1 /= dna2    --if these are equal, there is something wrong in our random generation, there is 1Mi combinations
            )

    describe "allSurvive selection" $ do
        it "doesn't change the population" $
            property ( \p i ->
                    let survivors = fst $ sampleState (allSurvive  p) $ mkStdGen i
                    in 
                        p == survivors
                )


    describe "extinction" $ do
        it "has no surviors" $
            property ( \p i ->
                let survivingPopulation = fst $ sampleState (individuals <$> extinction p) $ mkStdGen i
                in 
                    [] == survivingPopulation
            )

    describe "fittest" $ do
        it "produces population of limited size" $
            property ( \p i ->
                let survivingPopulation = fst $ sampleState (individuals <$> fittest 3 (\individual -> 1.0) p) $ mkStdGen i
                in 
                    3 >= length survivingPopulation
            )