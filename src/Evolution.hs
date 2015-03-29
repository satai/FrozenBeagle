module Evolution(step) where

import Data.Random
import Data.Functor

import Population

data Evolution = Evolution {
    selection :: Int -> Selection
}

step :: Population -> RVar Population
step p = allSurvive p


step' :: RVar Population -> RVar Population
step' p = do
    p' <- p
    step p'

evolution :: Population -> [(Int, RVar Population)]
evolution population = evolve  (0, return population)
     where
        evolve :: (Int, RVar Population) -> [(Int, RVar Population)]
        evolve (gen, pop) = ((gen, pop) : evolve (gen + 1, step' pop))