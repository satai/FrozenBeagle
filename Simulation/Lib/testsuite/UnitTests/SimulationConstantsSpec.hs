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

        it "accidentDeathProbability is probability" $
            isProbability accidentDeathProbability `shouldBe` True

        it "probabilityAlleleMutation is probability" $
            isProbability probabilityAlleleMutation `shouldBe` True

        it "maxSteps is positive" $
            maxSteps > 0

        it "optimumSizeCoefficient is positive" $
            optimumSizeCoefficient > 0

        it "optimumChangeSizeCoefficient is positive" $
            optimumChangeSizeCoefficient > 0

        it "nan is NaN" $
            nan `shouldSatisfy` isNaN

isProbability :: Double -> Bool
isProbability p = p >= 0.0 && p <= 1.0
