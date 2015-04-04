module UnitTests.EvolutionSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Random
import System.Random

import UnitTests.PopulationSpec()

import Evolution

spec :: Spec
spec = parallel $ do

    describe "evolution" $ do
         it "evolution step returns next generation" $
             property (\p i ->
                            let survivingPopulation = fst $ sampleState (step p) (mkStdGen i)
                            in 
                                p == survivingPopulation 
             )
