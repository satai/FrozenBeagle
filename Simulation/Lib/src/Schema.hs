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
    { schemaElements :: [Maybe Basis]
    } deriving (Eq)

newtype DominantSchema = DominantSchema
    { dominantSchemaElements :: [Maybe Basis]
    } deriving (Eq)

shortRepr :: Maybe Basis -> Char
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
    order (Schema elems) =  length $ filter (/= Nothing) elems

instance HasOrder DominantSchema
  where
    order (DominantSchema elems) =  length $ filter (/= Nothing) elems

instance MatchesGenes Schema
  where
    matches (Schema s) (DnaString d1) (DnaString d2) = matches' s d1 d2
      where
        matches' :: [Maybe Basis] -> [Basis] -> [Basis] -> Bool
        matches' [] [] []                             = True
        matches' (Nothing : bs) (_  : b1s) (_  : b2s) = matches' bs b1s b2s
        matches' (Just b  : bs) (b1 : b1s) (b2 : b2s) = (b == b1 || b == b2) && matches' bs b1s b2s
        matches' _ _ _                                = error "Incompatible Schema and DNA"

instance MatchesGenes DominantSchema
  where
    matches (DominantSchema s) (DnaString d1) (DnaString d2) = matches' s d1 d2
      where
        matches' :: [Maybe Basis] -> [Basis] -> [Basis] -> Bool
        matches' [] [] []                             = True
        matches' (Nothing : bs) (_  : b1s) (_  : b2s) = matches' bs b1s b2s
        matches' (Just b  : bs) (b1 : b1s) (b2 : b2s) = (b == b1 && b == b2) && matches' bs b1s b2s
        matches' _ _ _                                = error "Incompatible DominantSchema and DNA"
