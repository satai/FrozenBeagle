module Schema(Schema(Schema), order) where

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