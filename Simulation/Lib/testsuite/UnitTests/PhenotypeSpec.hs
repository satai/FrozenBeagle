{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.PhenotypeSpec (spec, Phenotype, Arbitrary) where

import Test.Hspec
import Test.HUnit.Approx
import Test.QuickCheck

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
                    "should equalequal"
                    0.001
                    (p1 `distance` p2 * abs q)
                    (Phenotype (map (* q) $ phenotypeToVector p1) `distance` Phenotype (map (* q) $ phenotypeToVector p2)))

        it "Phenotype has sane text representation" $
            show (Phenotype [0, 0, 1]) `shouldBe` "Phenotype [0.0,0.0,1.0]"

        it "increasing distance to the optimum decreaces fitness and via versa" $
            property (\optimum p1 p2 ->
               fitness optimum p1 `compare` fitness optimum p2
               ==
               distance optimum p2 `compare` distance optimum p1
           )
