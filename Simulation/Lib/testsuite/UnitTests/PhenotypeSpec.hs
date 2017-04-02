{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.PhenotypeSpec (spec, Phenotype, Arbitrary) where

import Test.Hspec
import Test.QuickCheck

import SimulationConstants
import Phenotype

instance Arbitrary Phenotype where
     arbitrary = Phenotype <$> vector dimensionCount

spec :: Spec
spec = parallel $ do
    describe "Phenotype" $ do
        it "Phenotype distance is euklidean one" $
          (distance (Phenotype [0, 0, 1]) (Phenotype [1, 0, 2])) `shouldBe` (sqrt 2)

        it "Distance of phenotype to itself is zero" $
            property (
                \p -> 0.0 == distance p p
            )

        it "Distance is symetric" $
            property (\p1 p2 -> (distance p1 p2) == (distance p2 p1))

        it "Phenotype has sane text representation" $
            (show (Phenotype [0, 0, 1])) `shouldBe` "Phenotype [0.0,0.0,1.0]"
