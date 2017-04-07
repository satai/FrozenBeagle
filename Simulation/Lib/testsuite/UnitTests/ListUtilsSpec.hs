{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.ListUtilsSpec( spec
                          ) where

import Test.Hspec
import Test.QuickCheck

import Control.Exception.Base

import Debug.Trace

import ListUtils

spec :: Spec
spec = parallel $ do
    describe "zipWithCheck" $ do
        it "produces empty list for two empty lists in input" $
            property (
                zipWithCheck (*) ([] :: [Int]) []
                    `shouldBe`
                []
            )

        it "behaves as zipWith for lists of the same length" $
            property ( \(NonEmpty list1') (NonEmpty list2') ->
                let
                    commonLength = min (length list1') (length list2')
                    list1 = take commonLength list1'
                    list2 = take commonLength list2'
                in
                    zipWith (+) (list1 :: [Int]) list2
                        `shouldBe`
                    zipWithCheck (+) list1 list2
            )

        it "fails in runtime when second list longer" $
            property ( \list ->
                 evaluate (traceShowId $ zipWithCheck (-) (list :: [Int]) (0 : list))
                    `shouldThrow`
                 errorCall "zipWithCheck and zipCheck require two lists of the same length"
            )

        it "fails in runtime when first list longer" $
            property ( \list ->
                 evaluate (traceShowId $ zipWithCheck (-) (0 : list) (list :: [Int]) )
                    `shouldThrow`
                 errorCall "zipWithCheck and zipCheck require two lists of the same length"
            )

    describe "zipWith" $ do
        it "produces empty list for two empty lists in input" $
            property (
                zipCheck ([] :: [Int]) ([] :: [String])
                    `shouldBe`
                ([] :: [(Int, String)])
            )

        it "behaves as zip for lists of the same length" $
            property ( \(NonEmpty list1') (NonEmpty list2') ->
                let
                    commonLength = min (length list1') (length list2')
                    list1 = take commonLength list1'
                    list2 = take commonLength list2'
                in
                    zip (list1 :: [Int]) (list2 :: [Double])
                        `shouldBe`
                    zipCheck list1 list2
            )

        it "fails in runtime when second list longer" $
            property ( \list ->
                evaluate (traceShowId $ zipCheck (list :: [String]) ("foo" : list))
                   `shouldThrow`
                errorCall "zipWithCheck and zipCheck require two lists of the same length"
            )

        it "fails in runtime when first list longer" $
            property ( \list ->
                evaluate (traceShowId $ zipCheck (0 : list) (list :: [Int]) )
                   `shouldThrow`
                errorCall "zipWithCheck and zipCheck require two lists of the same length"
            )