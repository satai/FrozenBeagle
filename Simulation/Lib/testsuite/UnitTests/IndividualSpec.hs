{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.IndividualSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Individual

import UnitTests.PopulationSpec()

spec :: Spec
spec = parallel $
    describe "Individual" $
        it "an individual equals itself" $
            property (\p1 -> (p1 :: Individual) == p1)
