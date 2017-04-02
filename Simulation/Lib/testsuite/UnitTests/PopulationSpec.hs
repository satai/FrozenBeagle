{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.PopulationSpec (spec, Arbitrary) where

import Test.Hspec
import Test.QuickCheck

import System.Random
import Data.Random
import qualified Data.MultiSet as MultiSet

import Population
import Phenotype
import Genes

import UnitTests.GenesSpec()
import UnitTests.PhenotypeSpec()

instance Arbitrary Individual where
    arbitrary = do
        s  <- elements [M, F]
        g  <- arbitrary
        d1 <- arbitrary
        d2 <- arbitrary
        p  <- arbitrary
        return $ Individual s g (d1, d2) p

instance Arbitrary Population where
    arbitrary = Population <$> arbitrary <*> arbitrary

instance Arbitrary Sex where
    arbitrary = elements [F, M]

spec :: Spec
spec = parallel $ do
    describe "Population" $ do

        it "individual has two different dna strings" $
            property ( \(Individual _ _ (dna1, dna2) _) ->
                dna1 /= dna2    --if these are equal, there is something wrong in our random generation, there is 1Mi combinations
            )

        it "consist of males and females" $
            property ( \p ->
                (MultiSet.fromList p) == (MultiSet.fromList $ (males p) ++ (females p))
            )

        it "males are all males" $
            property ( \p ->
                all (\i -> (sex i) == M) $ males p
            )

        it "females are all females" $
            property ( \p ->
                all (\i -> (sex i) == F) $ females p
            )

    describe "allSurvive selection" $
        it "doesn't change the population" $
            property ( \p i ->
                    let survivors = fst $ sampleState (allSurvive  p) $ mkStdGen i
                    in
                        p == survivors
                )

    describe "extinction" $
        it "has no surviors" $
            property ( \p i ->
                let survivingPopulation = fst $ sampleState (extinction p) $ mkStdGen i
                in
                    [] == survivingPopulation
            )

    describe "chosenPairs" $ do
        it "there are no chosen pairs when chosing from empty population" $
            property $ forAll (choose (1, 33)) (\fraction i ->
                let chosen = fst $ sampleState (chosenPairs fraction []) $ mkStdGen i
                in
                    [] == chosen
                )

        it "there is less chosen pairs when choosing a smaller fraction from a population" $
            property $ forAll (choose (3, 33)) (\fraction i p ->
            let
                chosen1 = fst $ sampleState (chosenPairs fraction p) $ mkStdGen i
                chosen2 = fst $ sampleState (chosenPairs 2        p) $ mkStdGen i
            in
                length chosen1 <= length chosen2
            )

    describe "fittest" $
        it "produces population of limited size" $
            property ( \p i ->
                let survivingPopulation = fst $ sampleState (fittest 3 (const 1.0) p) $ mkStdGen i
                in
                    3 >= length survivingPopulation
            )

    describe "hard selection" $ do
        it "keeps all members of population, that have fitness greater than treshold" $
            property ( \p i threshold ->
                let fitness' = const $ threshold + 0.1
                    survivingPopulation = fst $ sampleState (hardSelection fitness' threshold p) $ mkStdGen i
                in
                    survivingPopulation `shouldBe` p
            )

        it "kills all members of population, that have fitness smaller than treshold" $
            property ( \p i threshold ->
                let fitness' = const $ threshold - 0.1
                    survivingPopulation = fst $ sampleState (hardSelection fitness' threshold p) $ mkStdGen i
                in
                    survivingPopulation `shouldBe` []
            )

        it "kills all members of population, that are too old" $
            property ( \p i currentGeneration maximumAge ->
              let survivingPopulation = fst $ sampleState (killOld maximumAge currentGeneration p) $ mkStdGen i
                  age :: Individual -> Int
                  age individual = currentGeneration - birthGeneration individual
              in
                    survivingPopulation `shouldSatisfy` (\survivors -> null survivors || maximum (map age survivors) <= maximumAge)
            )

        it "keeps young enough individuals" $
          property ( \i ->
              let youngling = Individual {
                      sex=F,
                      birthGeneration=10,
                      chromosomes=(DnaString [], DnaString[]),
                      phenotype=Phenotype []
                  }
                  population = [youngling]
                  survivingPopulation = fst $ sampleState (killOld 5 13 population) $ mkStdGen i
              in
                    survivingPopulation `shouldBe` [youngling]
            )


        it "can keep only part of the population" $
            property ( \ populationPart1 populationPart2 i g1 g2 g3 g4 macho1 macho2 ->
                let fitness' :: Fitness
                    fitness' p
                        | macho1 == p = 100.0
                        | macho2 == p = 111.0
                        | otherwise  =   0.1
                    populationWithMacho = [Individual M 1 (g1, g2) macho1] ++ populationPart1 ++ [Individual M 0 (g3, g4) macho2] ++ populationPart2
                    survivingPopulation = fst $ sampleState (hardSelection fitness' 10.0 populationWithMacho) $ mkStdGen i
                in
                    map phenotype survivingPopulation `shouldBe` [macho1, macho2]
            )
