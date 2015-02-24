module Schema(Schema(Schema), order, matches) where

import Genes

data Schema = Schema [Maybe Basis] deriving (Eq)

instance Show Schema where
    show (Schema elems) = "{" ++ (map shortRepr elems) ++ "}"
    	where 
    		shortRepr :: Maybe Basis -> Char
    		shortRepr Nothing = head "*"
    		shortRepr (Just b) = head $ show b

order :: Schema -> Int
order (Schema elems) =  length $ filter (/= Nothing) elems

matches :: Schema -> DnaString -> Bool
matches (Schema s) (DnaString d) = all (== True) $ zipWith matches' s d
	where 
		matches' :: Maybe Basis -> Basis -> Bool
		matches' (Just b1) b2 = b1 == b2
		matches' (Nothing) _ = True