module UnitTests.SimulationSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Random
import Data.Maybe
import Data.List
import Data.Unique
import System.Random

import UnitTests.PopulationSpec()

import Simulation
import Phenotype
import Genes
import Schema

spec :: Spec
spec = parallel $ do
    describe "Simulation" $ do
        it "turbidostat constants" $
          property (turbidostatCoefiecientsForPopulationSize 0.1 10000 `shouldBe` 0.000000004)

        it "random pleiotropic rule should have schema of expected length" $
          property (\i ->
            (length $ schemaElements $ fst $  fst $ sampleState (randomPleiotropicRule 10) (mkStdGen i))
               `shouldBe`
            10
          )

        it "random pleiotropic rule should have fixed only one position of schema" $
          property (\i ->
            (length $ filter isJust $ schemaElements $ fst $ fst $ sampleState (randomPleiotropicRule 10) (mkStdGen i))
               `shouldBe`
            1
          )

        it "plenty of random pleiotropic rules should have fixed all posible positions in some rule" $
          property (\i ->
            (length $ nub $ map fromJust $ map (findIndex isJust) $ map schemaElements $ map fst $ fst $ sampleState (pleiotropicRules 10 100) (mkStdGen i))
               `shouldBe`
            10
          )

