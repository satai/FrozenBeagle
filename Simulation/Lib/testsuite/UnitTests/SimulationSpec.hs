module UnitTests.SimulationSpec(spec) where

import Test.Hspec
import Test.HUnit.Approx
import Test.QuickCheck

import Data.Random
import Data.Maybe
import Data.List
import Data.Set(fromList)
import System.Random

import UnitTests.PopulationSpec()

import Genes
import Individual
import Simulation.Internal
import Population.Internal
import Schema
import Phenotype
import SimulationConstants

spec :: Spec
spec = parallel $ do
    describe "Simulation" $ do
        it "turbidostat constants" $
          property (turbidostatCoefficientsForPopulationSize 0.1 10000 `shouldBe` 0.000000004)
--
--         it "random pleiotropic rule should have schema of expected length" $
--           property (\i ->
--             length (schemaElements $ fst $  fst $ sampleState (randomPleiotropicRule 10) (mkStdGen i))
--                `shouldBe`
--             10
--           )
--
--         it "random pleiotropic rule should have fixed only one position of schema" $
--           property (\i ->
--             length (filter isJust $ schemaElements $ fst $ fst $ sampleState (randomPleiotropicRule 10) (mkStdGen i))
--                `shouldBe`
--             1
--           )
--
--         it "plenty of random pleiotropic rules should have fixed all possible positions in some rule" $
--           property (\i ->
--             length (nub $ map (fromJust . findIndex isJust . schemaElements . fst) $ fst $ sampleState (pleiotropicRules 10 100) (mkStdGen i))
--                `shouldBe`
--             10
--           )

        it "optimum calculation produces constant for first couple of generations" $
            property $
                forAll ( choose (0, optimumChangeGeneration  - 1)) ( \i ->
                    optimumCalculation (Phenotype [1.0, 0.1, 0, 0]) (Phenotype [2.0, 0, 0, 0]) i
                    `shouldBe`
                    Phenotype [1.0, 0.1, 0, 0]
                )

        it "optimum calculation produces an other constant after for some time" $
            property $
                forAll ( choose (optimumChangeGeneration + 1, optimumChangeGeneration * 2 - 1)) ( \i ->
                    optimumCalculation (Phenotype [1.0, 0.1, 0, 0]) (Phenotype [2.0, 0, 0, 0]) i
                    `shouldBe`
                    Phenotype [2.0, 0, 0, 0]
                )


        it "optimum calculation produces the original constant ever after" $
            property $
                forAll ( choose (2 * optimumChangeGeneration  + 1, 10 * optimumChangeGeneration)) ( \i ->
                    optimumCalculation (Phenotype [1.0, 0.1, 0, 0]) (Phenotype [2.0, 0, 0, 0]) i
                    `shouldBe`
                    Phenotype [1.0, 0.1, 0, 0]
                )


        it "random population contains required number of individuals" $
            property (\i d (NonNegative count) ->
                length (individuals $ fst $ sampleState (randomPopulation count (\_ _ -> Phenotype []) d 33) (mkStdGen i))
                    `shouldBe`
                count
             )

        it "random population generation is 0" $
            property (\i d (NonNegative count) ->
                generation (fst $ sampleState (randomPopulation count (\_ _ -> Phenotype []) d 33)  (mkStdGen i))
                    `shouldBe`
                0
            )

        it "random population has only individuals of generation 0" $
            property (\i d (NonNegative count) ->
                map birthGeneration (individuals $ fst $ sampleState (randomPopulation count (\_ _ -> Phenotype []) d 23) (mkStdGen i))
                    `shouldBe`
                replicate count 0
            )

        it "big enough random population contains both Males and Females" $
            property (\i d (Positive count) ->
                fromList (map sex $ individuals $ fst $ sampleState (randomPopulation (count + 22) (\_ _ -> Phenotype []) d 13) (mkStdGen i))
                    `shouldBe`
                fromList [F, M]
            )

        it "big enough random population contains both Males and Females in about the same count" $
            -- https://en.wikipedia.org/wiki/Checking_whether_a_coin_is_fair#Examples
            property (\i d (Positive count) ->
                 let
                     n :: Int
                     n = count + 3000
                     is = individuals
                            $ fst
                            $ sampleState (randomPopulation n (\_ _ -> Phenotype []) d 33) (mkStdGen i)
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
            property (\i d (Positive count) (Positive geneCount) ->
                all (== (geneCount, geneCount))
                   $ map ((\ch -> (length $ genes $ fst ch, length $ genes $ snd ch)) . chromosomes)
                   $ individuals
                   $ fst
                   $ sampleState (randomPopulation (count + 22) (\_ _ -> Phenotype []) d geneCount) (mkStdGen i)
            )

--         it "random rules have expected count of rules" $
--             property (\i (Positive baseCount') (NonNegative pleiotropicRulesCount') (NonNegative epistaticRulesCount') (NonNegative complicatedRulesCount') ->
--                 length (fst $ sampleState (randomRules baseCount' pleiotropicRulesCount' epistaticRulesCount' complicatedRulesCount') (mkStdGen i))
--                     `shouldBe`
--                 3 * baseCount' + pleiotropicRulesCount' + epistaticRulesCount' + complicatedRulesCount'
--             )

    describe "Collapse" $ do
        it "for the same seed is the result the same" $
            property ( \seed' ->
                collapse seed' (uniform (1 :: Integer) 1000000000000)
                    `shouldBe`
                collapse seed' (uniform (1 :: Integer) 1000000000000)
            )

        it "for the different seeds the result is not the same" $
            property ( \seed' ->
                collapse seed' (uniform (1 :: Integer) 1000000000000)
                    `shouldNotBe`
                collapse (succ seed') (uniform (1 :: Integer) 1000000000000)
            )

    describe "Average" $ do
        it "zero for list of zeros" $
            property (
                assertApproxEqual
                    "avg should be zero for list of zeros"
                    0.000001
                    (average [0.0, 0.0, 0.0])
                    0.0
            )

        it "x for [x]" $
            property ( \x ->
                 assertApproxEqual
                    "avg should be x for [x]"
                    0.000001
                    (average [x])
                    x
            )

        it "2 for 1,2,3,2" $
            property (
                 assertApproxEqual
                    "avg should be 2 for 1,2,3,2"
                    0.000001
                    (average [1.0, 2.0, 3.0, 2.0])
                    2.0
            )

        it "the same for a list when elements duplicated" $
            property ( \(NonEmpty xs) ->
                assertApproxEqual
                    "avgs should be the same for a list when elements duplicated"
                    0.000001
                    (average (xs ++ xs))
                    (average xs)
            )

        it "changes d-times if all elements of the list are multiplied by d" $
            property ( \(NonEmpty xs) d ->
                assertApproxEqual
                    "avg changes d-times if all elements of the list are multiplied by d"
                    0.000001
                    (d * average xs)
                    (average (map (* d) xs))
            )

        it "is NaN for empty list" $
            property (
                average []
                    `shouldSatisfy`
                isNaN
            )

    describe "Standard deviation" $ do
        it "zero for list of zeroes" $
            property (
                assertApproxEqual
                    "stdDev should be zero for list of zeros"
                    0.000001
                    (stdDev [0.0, 0.0, 0.0])
                    0.0
            )

        it "zero for list of the same values" $
            property (\x (Positive n) ->
                assertApproxEqual
                    "stdDev should be zero for list of zeros"
                    0.000001
                    (stdDev $ replicate n x)
                    0.0
            )

        it "x for [x]" $
            property ( \x ->
                 assertApproxEqual
                    "stdDev should be 0.0 for [x]"
                    0.000001
                    (stdDev [x])
                    0.0
            )

        it "2 for 1,2,3,2" $
            property (
                 assertApproxEqual
                    "stdDev should be 0.70711 for 1,2,3,2"
                    0.000001
                    (stdDev [1.0, 2.0, 3.0, 2.0])
                    0.7071067811865476
            )

        it "the same for a list when elements duplicated" $
            property ( \(NonEmpty xs) ->
                assertApproxEqual
                    "stdDev should be the same for a list when elements duplicated"
                    0.000001
                    (stdDev (xs ++ xs))
                    (stdDev xs)
            )

        it "changes d-times if all elements of the list are multiplied by d" $
            property ( \(NonEmpty xs) d ->
                assertApproxEqual
                    "stdDev changes (abs d)-times if all elements of the list are multiplied by d"
                    0.000001
                    (abs d * stdDev xs)
                    (stdDev (map (* d) xs))
            )

        it "is NaN for empty list" $
            property (
                stdDev []
                    `shouldSatisfy`
                isNaN
            )


    describe "All the same" $ do
        it "true for list of the same values" $
            property (\x (Positive n) ->
                allTheSame (replicate n (x :: Int))
                    `shouldBe`
                True
            )

        it "false for list with some different value" $
            property (\x (Positive n) ->
                allTheSame ((x + 1) : replicate n (x :: Int))
                    `shouldBe`
                False
            )

        it "true for empty list" $
            property (
                allTheSame ([] :: [String])
                    `shouldBe`
                True
            )


    describe "Almost all the same" $ do
        it "true for list of the same values" $
            property (\x (Positive n) ->
                almostAllTheSame (replicate n (x :: Int))
                    `shouldBe`
                True
            )

        it "false for list with some different value" $
            property (\x (Positive n) ->
                almostAllTheSame (replicate n (x + 1)  ++ replicate (n * 8) (x :: Int))
                    `shouldBe`
                False
            )

        it "true for list with only a few different value" $
            property (\x (Positive n) ->
                almostAllTheSame ((x + 1)  : replicate (n + 11) (x :: Int))
                    `shouldBe`
                True
            )

        it "true for list with too few different values" $
            property (\x (Positive n) ->
                almostAllTheSame ((x + 1)  : replicate (n * 9 + 22) (x :: Int) ++ replicate n (x + 1))
                    `shouldBe`
                True
            )

        it "true for empty list" $
            property (
                almostAllTheSame ([] :: [String])
                    `shouldBe`
                True
            )
