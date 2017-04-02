module UnitTests.SimulationSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Random
import Data.Maybe
import Data.List
import System.Random

import UnitTests.PopulationSpec()

import Simulation
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

