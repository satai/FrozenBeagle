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

matches :: Schema -> DnaString -> DnaString -> Bool
matches (Schema s) (DnaString d1) (DnaString d2) = matches' s d1 d2
  where
    matches' :: [Maybe Basis] -> [Basis] -> [Basis] -> Bool
    matches' [] [] []                             = True
    matches' (Nothing : bs) (_  : b1s) (_  : b2s) = matches' bs b1s b2s
    matches' (Just b  : bs) (b1 : b1s) (b2 : b2s) = (b == b1 || b == b2) && matches' bs b1s b2s
    matches' _ _ _                                = error "Incompatible Schema and DNA"
