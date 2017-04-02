{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.EmptySpec ( spec
                           ) where

import Test.Hspec

spec :: Spec
spec = parallel $
    describe "empty spec" $
        it "placehpolder for future specs" $
            "1" == "1"