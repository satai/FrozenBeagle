module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Verify tests are run" $ do
    it "equals zero" $ do
      123 `shouldBe` 123