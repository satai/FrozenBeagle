module Schema
    ( Schema (Schema)
    , order
    , matches
    , schemaElements
    ) where

import           Genes

newtype Schema = Schema
    { schemaElements :: [Maybe Basis]
    } deriving (Eq)

instance Show Schema
  where
    show (Schema elems) = "{" ++ map shortRepr elems ++ "}"
      where
        shortRepr :: Maybe Basis -> Char
        shortRepr Nothing  = '*'
        shortRepr (Just b) = head $ show b

order :: Schema -> Int
order (Schema elems) =  length $ filter (/= Nothing) elems

matches :: Schema -> DnaString -> Bool
matches (Schema s) (DnaString d) = matches' s d
  where
    matches' :: [Maybe Basis] -> [Basis] -> Bool
    matches' [] []                     = True
    matches' (Nothing : b1s) (_ : b2s) = matches' b1s b2s
    matches' (Just b1: b1s) (b2:b2s)   = b1 == b2 && matches' b1s b2s
    matches' _ _                       = error "Incopatible Schema and DNA"
