module UnitTests.EvolutionSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor
import Data.List

import UnitTests.PopulationSpec(Arbitrary)

import Evolution

spec :: Spec
spec = parallel $ do

    describe "evolution" $ do
        it "evolution step increases generation number" $
            property ( \p (NonNegative i) ->
                (succ i) == (fst $ step i p)
                )

        