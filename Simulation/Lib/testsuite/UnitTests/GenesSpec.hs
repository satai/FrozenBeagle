{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.GenesSpec (spec, DnaString, Allele) where

import Test.Hspec
import Test.QuickCheck

import Data.Random
import System.Random

import Data.List

import Genes
import ListUtils
import Phenotype

import UnitTests.PhenotypeSpec()

instance Arbitrary Allele where
  arbitrary = Allele <$> arbitrary <*> arbitrary

instance Arbitrary DnaString where
     arbitrary = DnaString <$> vector 8

newtype PointInString = PointInString Int deriving Show
instance Arbitrary PointInString  where
     arbitrary = PointInString <$> elements [0..8]

newtype IndexInString = IndexInString Int deriving Show
instance Arbitrary IndexInString  where
     arbitrary = IndexInString <$> elements [0..7]

spec :: Spec
spec = parallel $ do
    describe "Allelas" $ do
         it "alella equals itself" $
            property (
                \a -> (a:: Allele) == a
            )

         it "alellas are ordered" $
             property (
                 \e de1 de2 ->
                     Allele e de1 `compare` Allele e de2
                     `shouldBe`
                     de1 `compare` de2
             )

         it "alellas are ordered" $
             property (
                 \e1 e2 de ->
                     Allele e1 de `compare` Allele e2 de
                     `shouldBe`
                     e1 `compare` e2
             )


    describe "Genes" $ do

        it "show DNA string looks like '[12213]'" $
             show ( DnaString [ Allele (Phenotype [ 1.1, 1.2]) (Phenotype [-1.2, 0.3]),
                                Allele (Phenotype [-1.2, 0.3]) (Phenotype [-0.2, 0.3])
                              ]
                  ) `shouldBe` "[{(1.1,1.2)|(-1.2,0.3)}, {(-1.2,0.3)|(-0.2,0.3)}]"

        it "show DNA strings are lexicographicaly ordered" $
             DnaString [ Allele (Phenotype [ 1.1, 1.2]) (Phenotype [-1.2, 0.3]),
                         Allele (Phenotype [-1.2, 0.3]) (Phenotype [-0.2, 0.3])
                       ]
             `compare`
             DnaString [ Allele (Phenotype [ 1.1, 1.2]) (Phenotype [-1.2, 0.3]),
                         Allele (Phenotype [-1.2, 5.5]) (Phenotype [-0.2, 0.3])
                       ]
                 `shouldBe`
             LT

        it "dna equals itself" $
            property ( \(DnaString dna) ->
                DnaString dna == DnaString dna
                    `shouldBe`
                True
            )

        it "length of crosovered dna is the same as the mother dnas" $
            property ( \(DnaString dna1) (DnaString dna2)  ->
                length dna1
                    ==
                length (genes $ crossover 4 (DnaString dna1) (DnaString dna2))
            )

        it "beginning of crosovered DNA is from the first mother string" $
            property ( \(PointInString n) (DnaString dna1) (DnaString dna2)  ->
                take n dna1
                    ==
                take n (genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "end of crosovered DNA is from the second mother string" $
            property ( \(PointInString n) (DnaString dna1) (DnaString dna2)  ->
                drop n dna2
                    ==
                drop n (genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "crosovered DNA is of the same length as mother dnas" $
            property ( \(PointInString n) (DnaString dna1) (DnaString dna2)  ->
                length dna1
                    ==
                length (genes $ crossover n (DnaString dna1) (DnaString dna2))
            )

        it "mutated dna has same length as dna before mutation" $
            property ( \(IndexInString n) b (DnaString dna) ->
                length dna
                    ==
                length (genes $ mutate n b (DnaString dna))
            )

        it "mutated dna has new Allele at point of mutation" $
            property ( \(IndexInString n)  b  (DnaString dna ) ->
                b == genes (mutate n b (DnaString dna)) !! n
            )

        it "mutated dna doesn't differ from original one with exception of point of mutation" $
            property ( \(IndexInString n) b (DnaString dna ) ->
                 1 >= length (elemIndices True $ zipWithCheck (/=) dna (genes $ mutate n b (DnaString dna)) )
            )

    describe "Random Dominant Effect" $ do
        it "there is no effect if it's probability is 0.0" $
            property ( \(Phenotype p) seed  ->
                fst (sampleState (randomDominantEffect (Phenotype p) 0.0) (mkStdGen seed))
                    `shouldBe`
                Phenotype p
            )

        it "result is oposite direction than input if probability is 1.0" $
            property ( \(Phenotype p) seed  ->
                fst (sampleState (randomDominantEffect (Phenotype p) 1.0) (mkStdGen seed))
                    `shouldSatisfy`
                ( \(Phenotype p') ->
                   all (== 0) (zipWith (+) (map signum p) (map signum p'))
                )
            )