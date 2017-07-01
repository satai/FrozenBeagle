{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.PhenotypeSpec (spec, Phenotype, Arbitrary) where

import Test.Hspec
import Test.HUnit.Approx
import Test.QuickCheck

import Control.Exception.Base
import Data.Random
import System.Random

import Debug.Trace

import SimulationConstants
import Phenotype.Internal

instance Arbitrary Phenotype where
     arbitrary = Phenotype <$> vector dimensionCount

spec :: Spec
spec = parallel $
    describe "Phenotype" $ do
        it "Phenotype distance is euklidean one" $
            Phenotype [0, 0, 1] `distance` Phenotype [1, 0, 2] `shouldBe` sqrt 2

        it "Distance of phenotype to itself is zero" $
            property (
                \p -> 0.0 == p `distance` p
            )

        it "Distance is symetric" $
            property (\p1 p2 -> p1 `distance` p2 == p2 `distance` p1)

        it "Distance is not negative" $
            property (\p1 p2 -> p1 `distance` p2 >= 0.0)

        it "Distance is a linear form" $
            property (\p1 p2 q ->
                assertApproxEqual
                    "should equal"
                    0.001
                    (p1 `distance` p2 * abs q)
                    (Phenotype (map (* q) $ phenotypeToVector p1) `distance` Phenotype (map (* q) $ phenotypeToVector p2)))

        it "euclidean distance fails in runtime for vectors of different dimensions" $
            property ( \(NonEmpty p) ->
                evaluate (traceShowId $ Phenotype p `distance` Phenotype [])
                   `shouldThrow`
                anyErrorCall
            )

        it "increasing distance to the optimum decreaces fitness and via versa" $
            property (\optimum p1 p2 ->
                (
                    fitness optimum p2 == 0.0
                        &&
                    fitness optimum p1 == 0.0
                        &&
                    distance optimum p2 > 100.0  --FIXME
                        &&
                    distance optimum p1 > 100.0  --FIXME
                )
                  ||
                (
                    distance optimum p2 `compare` distance optimum p1
                       ==
                    fitness optimum p1 `compare` fitness optimum p2
                )
            )

        it "Phenotype has sane text representation" $
            show (Phenotype [0, 0, 1]) `shouldBe` "(0.0,0.0,1.0)"

        it "zeroPhenotype contains only zeroes" $
            all (== 0.0) (phenotypeToVector zeroPhenotype) `shouldBe` True

        it "zeroPhenotype length is the dimension" $
            length (phenotypeToVector zeroPhenotype) `shouldBe` dimensionCount

        it "randomPhenotypeChange length is the dimension" $
            property (\i ->
                     length (phenotypeToVector $ fst $ sampleState randomPhenotypeChange (mkStdGen i))
                        `shouldBe`
                     dimensionCount
            )

        it "randomPhenotypeChangeWithOneNonzero length is the dimension" $
            property (\i ->
                     length (phenotypeToVector $ fst $ sampleState randomPhenotypeChangeWithOneNonzero (mkStdGen i))
                        `shouldBe`
                     dimensionCount
            )

        it "randomPhenotypeChangeWithOneNonzero has one non-zero" $
            property (\i ->
                     length (filter (/= 0.0) $ phenotypeToVector $ fst $ sampleState randomPhenotypeChangeWithOneNonzero (mkStdGen i))
                        `shouldBe`
                     1
            )
