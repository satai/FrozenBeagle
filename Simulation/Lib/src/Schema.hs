{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Schema
    ( MatchesGenes
    , Schema (..)
    , DominantSchema (..)
    , order
    , matches
    ) where

import           Genes

class MatchesGenes a where
    matches :: a -> DnaString -> DnaString -> Bool

class HasOrder a where
    order :: a -> Int

newtype Schema = Schema
    { schemaElements :: [Maybe Alela]
    } deriving (Eq)

newtype DominantSchema = DominantSchema
    { dominantSchemaElements :: [Maybe Alela]
    } deriving (Eq)

shortRepr :: Maybe Alela -> Char
shortRepr Nothing  = '*'
shortRepr (Just b) = head $ show b

instance Show Schema
  where
    show (Schema elems) = "{" ++ map shortRepr elems ++ "}"

instance Show DominantSchema
  where
    show (DominantSchema elems) = "(" ++ map shortRepr elems ++ ")"

instance HasOrder Schema
  where
    order = length . filter (/= Nothing) . schemaElements

instance HasOrder DominantSchema
  where
    order = length . filter (/= Nothing) . dominantSchemaElements

instance MatchesGenes Schema
  where
    matches (Schema s) (DnaString d1) (DnaString d2) =
      if length s == length d1 && length d1 == length d2
      then matches' s d1 d2
      else error "Incompatible Schema and DNA"
      where
        matches' :: [Maybe Alela] -> [Alela] -> [Alela] -> Bool
        matches' [] [] []                             = True
        matches' (Nothing : bs) (_  : b1s) (_  : b2s) = matches' bs b1s b2s
        matches' (Just b  : bs) (b1 : b1s) (b2 : b2s) = (b == b1 || b == b2) && matches' bs b1s b2s

instance MatchesGenes DominantSchema
  where
    matches (DominantSchema s) (DnaString d1) (DnaString d2) =
      if length s == length d1 && length d1 == length d2
      then matches' s d1 d2
      else error "Incompatible DominantSchema and DNA"
      where
        matches' :: [Maybe Alela] -> [Alela] -> [Alela] -> Bool
        matches' [] [] []                             = True
        matches' (Nothing : bs) (_  : b1s) (_  : b2s) = matches' bs b1s b2s
        matches' (Just b  : bs) (b1 : b1s) (b2 : b2s) = (b == b1 && b == b2) && matches' bs b1s b2s
