module Monads2 where

import Prelude hiding (Monad, (>>=), return, sum, pairs, ST)

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad Maybe where
    -- (>>=) :: m a -> (a -> m b) -> m b
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x
    -- return :: a -> m a
    return         = pure -- or, return x = Just x

instance Monad [] where
    -- (>>=) :: m a -> (a -> m b) -> m b
    xs >>= f = concat (fmap f xs) -- concatMap f xs, or [ y | x <- xs, y <- f x ]
    -- return :: a -> m a
    return x = [x]

sum :: Maybe Int
sum = do x <- Just 1
         y <- Just 2
         z <- Just 3
         return (x+y+z)

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)
                 -- [ (x, y) | x <- xs, y <- ys ]

pairs' :: [a] -> [b] -> [(a,b)]
pairs' xs ys = [ (x, y) | x <- xs, y <- ys ]

-- pairs above is specific to lists
-- mPairs here generalises for all monad types, including lists
mPairs :: (Monad m) => m a -> m b -> m (a,b)
mPairs mx my = mx >>= \x ->
               my >>= \y ->
               return (x,y)
-- Explicitly using >>= (bind) here as do notation uses GHC Monad type

main = do print sum
          print $ pairs [1,2] [3,4]
          print $ pairs' [1,2] [3,4]
          print $ mPairs [1,2,3] [4,5,6]
          print $ mPairs (Just 5) (Just 6)