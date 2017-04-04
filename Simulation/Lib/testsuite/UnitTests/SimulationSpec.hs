module UnitTests.SimulationSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Random
import Data.Maybe
import Data.List
import Data.Set(fromList)
import System.Random

import UnitTests.PopulationSpec()

import Genes
import Simulation
import Population
import Schema
import Phenotype
import SimulationConstants

spec :: Spec
spec = parallel $
    describe "Simulation" $ do
        it "turbidostat constants" $
          property (turbidostatCoefficientsForPopulationSize 0.1 10000 `shouldBe` 0.000000004)

        it "random pleiotropic rule should have schema of expected length" $
          property (\i ->
            length (schemaElements $ fst $  fst $ sampleState (randomPleiotropicRule 10) (mkStdGen i))
               `shouldBe`
            10
          )

        it "random pleiotropic rule should have fixed only one position of schema" $
          property (\i ->
            length (filter isJust $ schemaElements $ fst $ fst $ sampleState (randomPleiotropicRule 10) (mkStdGen i))
               `shouldBe`
            1
          )

        it "plenty of random pleiotropic rules should have fixed all posible positions in some rule" $
          property (\i ->
            length (nub $ map (fromJust . findIndex isJust . schemaElements . fst) $ fst $ sampleState (pleiotropicRules 10 100) (mkStdGen i))
               `shouldBe`
            10
          )

        it "optimum calculation produces constant for first couple of generations" $
            property $
                forAll ( choose (0, optimumChangeGeneration  - 1)) ( \i ->
                    optimumCalculation (Phenotype [1.0, 0.1, 0, 0]) (Phenotype [2.0, 0, 0, 0]) i
                    `shouldBe`
                    Phenotype [1.0, 0.1, 0, 0]
                )

        it "optimum calculation produces an other constant after" $
            property $
                forAll ( choose (optimumChangeGeneration, optimumChangeGeneration * 10)) ( \i ->
                    optimumCalculation (Phenotype [1.0, 0.1, 0, 0]) (Phenotype [2.0, 0, 0, 0]) i
                    `shouldBe`
                    Phenotype [2.0, 0, 0, 0]
                )


        it "random population contains required number of individuals" $
            property (\i (Positive count) ->
                length (individuals $ fst $ sampleState (randomPopulation count (\_ _ -> Phenotype []) 33) (mkStdGen i))
                    `shouldBe`
                count
             )

        it "random population generation is 0" $
            property (\i (Positive count) ->
                generation (fst $ sampleState (randomPopulation count (\_ _ -> Phenotype []) 33) (mkStdGen i))
                    `shouldBe`
                0
            )

        it "random population has only individuals of generation 0" $
            property (\i (Positive count) ->
                map birthGeneration (individuals $ fst $ sampleState (randomPopulation count (\_ _ -> Phenotype []) 23) (mkStdGen i))
                    `shouldBe`
                replicate count 0
            )

        it "big enough random population contains both Males and Females" $
            property (\i (Positive count) ->
                fromList (map sex $ individuals $ fst $ sampleState (randomPopulation (count + 22) (\_ _ -> Phenotype []) 13) (mkStdGen i))
                    `shouldBe`
                fromList [F, M]
            )

        it "big enough random population contains both Males and Females in about the same count" $
            -- https://en.wikipedia.org/wiki/Checking_whether_a_coin_is_fair#Examples
            property (\i (Positive count) ->
                 let
                     n :: Int
                     n = count + 3000
                     is = individuals
                            $ fst
                            $ sampleState (randomPopulation n (\_ _ -> Phenotype []) 33) (mkStdGen i)
                     femCount :: Int
                     femCount = length $ filter (== F) $ map sex is
                     p :: Double
                     p = fromIntegral femCount / fromIntegral n
                     r = 0.5
                     e :: Double
                     e = 4.4172 / 2.0 / sqrt (fromIntegral n)
                 in
                     (p - e < r) && (r < p + e)
            )

        it "random population has all the chromosomes of the expected length" $
            property (\i (Positive count) (Positive geneCount) ->
                all (== (geneCount, geneCount))
                   $ map (\ch -> (length $ genes $ fst ch, length $ genes $ snd ch))
                   $ map chromosomes
                   $ individuals
                   $ fst
                   $ sampleState (randomPopulation (count + 22) (\_ _ -> Phenotype []) geneCount) (mkStdGen i)
            )