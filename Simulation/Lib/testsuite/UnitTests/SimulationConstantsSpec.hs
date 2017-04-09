module UnitTests.SimulationConstantsSpec( spec
                                        ) where

import Test.Hspec

import SimulationConstants

spec :: Spec
spec = parallel $
    describe "SimulationConstants" $ do
        it "dimensionCount is at least 1" $
            dimensionCount >= 1 `shouldBe` True

        it "zeroPhenotypeVec is the right dimension" $
            length zeroPhenotypeVec `shouldBe` dimensionCount

        it "accidentDeathProbability is pribability" $
            isProbability accidentDeathProbability `shouldBe` True

        it "probabilityIndividualMutation is pribability" $
            isProbability probabilityIndividualMutation `shouldBe` True

        it "probabilityBasisMutation is pribability" $
            isProbability probabilityBasisMutation `shouldBe` True

        it "maximumCountOfOffspring is at least 1" $
            maximumCountOfOffspring >= 1 `shouldBe` True

        it "nan is NaN" $
            nan `shouldSatisfy` isNaN

isProbability :: Double -> Bool
isProbability p = p >= 0.0 && p <= 1.0
