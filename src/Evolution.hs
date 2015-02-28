module Evolution(step) where

import Data.Random

import Population

data Evolution = Evolution {
    selection :: Int -> Selection
}

step :: Int -> Population -> (Int, RVar Population)
step n p = (n + 1, allSurvive p)
