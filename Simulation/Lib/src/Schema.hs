{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Schema
    ( MatchesGenes
    , Schema (..)
    , order
    , matches
    ) where

import           Genes

class MatchesGenes a where
    matches :: a -> DnaString -> DnaString -> Bool

class HasOrder a where
    order :: a -> Int

newtype Schema = Schema
    { schemaElements :: [Maybe Allele]
    } deriving (Eq)

shortRepr :: Maybe Allele -> Char
shortRepr Nothing  = '*'
shortRepr (Just b) = head $ show b

instance Show Schema
  where
    show (Schema elems) = "<" ++ map shortRepr elems ++ ">"

instance HasOrder Schema
  where
    order = length . filter (/= Nothing) . schemaElements

instance MatchesGenes Schema
  where
    matches (Schema s) (DnaString d1) (DnaString d2) =
      if length s == length d1 && length d1 == length d2
      then matches' s d1 d2
      else error "Incompatible Schema and DNA"
      where
        matches' :: [Maybe Allele] -> [Allele] -> [Allele] -> Bool
        matches' [] [] []                             = True
        matches' (Nothing : bs) (_  : b1s) (_  : b2s) = matches' bs b1s b2s
        matches' (Just b  : bs) (b1 : b1s) (b2 : b2s) = (b == b1 || b == b2) && matches' bs b1s b2s