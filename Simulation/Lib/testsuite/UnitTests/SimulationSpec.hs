module UnitTests.SimulationSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Random
import System.Random

import UnitTests.PopulationSpec()

import Simulation

spec :: Spec
spec = parallel $ do
    describe "simulation" $ do
        it "turbidostat " $
          property (turbidostatCoefiecientsForPopulationSize 0.1 10000 `shouldBe` 0.000000005, 0.1)

