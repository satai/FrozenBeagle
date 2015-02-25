module UnitTests.PhenotypeSpec (spec, Phenotype) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Data.List

import Genes
import Phenotype

instance Arbitrary Phenotype where
     arbitrary = Phenotype <$> vector 10

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
