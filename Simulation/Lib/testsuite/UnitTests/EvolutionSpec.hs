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
        it "evolution step returns the population with the same individuals and incremented generation when evolutionary operations are identities" $
            property (\p i ->
                            let
                                nothingHappens :: EvolutionRules
                                nothingHappens = EvolutionRules { mutation = return, breeding = \_ -> return, selection = \_ -> return, deaths = \_ -> return }
                                generations :: RVar Population -> RVar [Population]
                                generations = evolution 10 nothingHappens
                                populationAfter2Generations = (fst $ sampleState (generations $ return $ Population 0 p) (mkStdGen i)) !! 2
                            in
                                populationAfter2Generations `shouldBe` Population 2 p
             )

        it "evolution step returns empty population when extinction happens" $
            property (\p i ->
                            let
                                extinctionHappens :: EvolutionRules
                                extinctionHappens = EvolutionRules{mutation = return, breeding = \_ -> return, selection = \_ -> extinction, deaths = \_ -> return}
                                generations :: RVar Population -> RVar [Population]
                                generations = evolution 10 extinctionHappens
                                populationAfter2Generations = (fst $ sampleState (generations $ return p) (mkStdGen i)) !! 2
                            in
                                individuals populationAfter2Generations `shouldBe` []
            )
