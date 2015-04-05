module UnitTests.PopulationSpec (spec, Arbitrary) where

import Test.Hspec
import Test.QuickCheck

import System.Random
import Data.Functor
import Data.Random
import qualified Data.MultiSet as MultiSet

import Population

import UnitTests.GenesSpec()

instance Arbitrary Individual where
    arbitrary = do
        s <- elements [M, F]
        d1 <- arbitrary
        d2 <- arbitrary
        return $ Individual s (d1, d2)
      
instance Arbitrary Population where
    arbitrary = Population <$> arbitrary 

spec :: Spec
spec = parallel $ do
    describe "Population" $ do

        it "individual has two different dna strings" $
            property ( \(Individual _ (dna1, dna2)) ->
                dna1 /= dna2    --if these are equal, there is something wrong in our random generation, there is 1Mi combinations
            )

        it "consist of males and females" $
            property ( \p ->
                (MultiSet.fromList $ individuals p) == (MultiSet.fromList $ (males p) ++ (females p))
            )

        it "males are all males" $
            property ( \p ->
                all (\i -> (sex i) == M) $ males p
            )

        it "females are all females" $
            property ( \p ->
                all (\i -> (sex i) == F) $ females p
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
                let survivingPopulation = fst $ sampleState (individuals <$> fittest 3 (const 1.0) p) $ mkStdGen i
                in 
                    3 >= length survivingPopulation
            )

    describe "hard selection" $ do
        it "keeps all members of population, that have fitness greater than treshold" $
            property ( \p i treshold ->
                let fitness = const $ treshold + 0.1
                    survivingPopulation = fst $ sampleState (individuals <$> hardSelection fitness treshold p) $ mkStdGen i
                in
                    Population survivingPopulation `shouldBe` p
            )

        it "kills all members of population, that have fitness smaller than treshold" $
            property ( \p i treshold ->
                let fitness = const $ treshold - 0.1
                    survivingPopulation = fst $ sampleState (individuals <$> hardSelection fitness treshold p) $ mkStdGen i
                in
                    survivingPopulation `shouldBe` []
            )

