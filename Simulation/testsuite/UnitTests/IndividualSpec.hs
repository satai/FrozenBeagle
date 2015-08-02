{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.IndividualSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Hashable
import Individual

import UnitTests.PopulationSpec(Arbitrary)

spec :: Spec
spec = parallel $ do
    describe "Individual" $ do
        it "Two individuals have the same hash iff they equal" $
            property (\p1 p2 -> (p1 == p2) == ((hashWithSalt 0 (p1::Individual) ) == (hashWithSalt 0 (p2::Individual) )))
