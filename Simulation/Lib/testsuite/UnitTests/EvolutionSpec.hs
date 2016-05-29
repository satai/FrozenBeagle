module UnitTests.EvolutionSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Random
import System.Random

import UnitTests.PopulationSpec()

import Evolution
import Population

spec :: Spec
spec = parallel $ do
    describe "evolution" $ do
        it "evolution step returns the same population when evolutionary operations are identities" $
            property (\p i ->
                            let
                                nothingHappens :: EvolutionRules
                                nothingHappens = EvolutionRules{mutation = return, breeding = return, selection = return}
                                generations :: RVar Population -> [RVar Population]
                                generations = evolution nothingHappens
                                populationAfter2Generations = fst $ sampleState ((generations $ return p) !! 2) (mkStdGen i)
                            in
                                populationAfter2Generations `shouldBe` p
             )

        it "evolution step returns empty population when extinction happens" $
            property (\p i ->
                            let
                                extinctionHappens :: EvolutionRules
                                extinctionHappens = EvolutionRules{mutation = return, breeding = return, selection = extinction}
                                generations :: RVar Population -> [RVar Population]
                                generations = evolution extinctionHappens
                                populationAfter2Generations = fst $ sampleState ((generations $ return  p) !! 2) (mkStdGen i)
                            in
                                populationAfter2Generations `shouldBe` Population []
            )
