module ListUtils( zipWithCheck
                , zipCheck
                ) where

zipWithCheck :: (a->b->c) -> [a]->[b]->[c]
zipWithCheck _  []     []     = []
zipWithCheck _  []     _      = error "zipWithCheck and zipCheck require two lists of the same length"
zipWithCheck _  _      []     = error "zipWithCheck and zipCheck require two lists of the same length"
zipWithCheck f (a:as) (b:bs) = f a b : zipWithCheck f as bs


zipCheck :: [a]->[b]->[(a,b)]
zipCheck = zipWithCheck (,)
