{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.PopulationSpec (spec, Arbitrary) where

import Test.Hspec
import Test.QuickCheck

import System.Random
import Data.Random
import qualified Data.MultiSet as MultiSet

import Population

import UnitTests.GenesSpec()
import UnitTests.PhenotypeSpec()

instance Arbitrary Individual where
    arbitrary = do
        s <- elements [M, F]
        d1 <- arbitrary
        d2 <- arbitrary
        p <- arbitrary
        return $ Individual s (d1, d2) p

instance Arbitrary Population where
    arbitrary = Population <$> arbitrary

instance Arbitrary Sex

spec :: Spec
spec = parallel $ do
    describe "Population" $ do

        it "individual has two different dna strings" $
            property ( \(Individual _ (dna1, dna2) _) ->
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

        it "can keep only part of the population" $
            property ( \ populationPart1 populationPart2 i g1 g2 g3 g4 macho1 macho2 ->
                let fitness :: Fitness
                    fitness p
                        | macho1 == p = 100.0
                        | macho2 == p = 111.0
                        | otherwise            =   0.1
                    populationWithMacho = Population $ [Individual M (g1, g2) macho1] ++ populationPart1 ++ [Individual M (g3, g4) macho2] ++ populationPart2
                    survivingPopulation = fst $ sampleState (individuals <$> hardSelection fitness 10.0 populationWithMacho) $ mkStdGen i
                in
                    map phenotype survivingPopulation `shouldBe` [macho1, macho2]
            )
