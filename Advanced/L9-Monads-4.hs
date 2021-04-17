module Monads4 where

import Prelude hiding (map, mapM, join)
import Data.Char ( isDigit, digitToInt )

-- generic monadic functions

-- map for lists of non-monadic values
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

-- generic map for lists of monadic values, do
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ []     = return []
mapM f (x:xs) = do y <- f x
                   ys <- mapM f xs
                   return (y:ys)

-- generic map for lists of monadic values, >>=
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ []     = return []
mapM' f (x:xs) = f x >>= \y ->
                 mapM f xs >>= \ys ->
                 return (y:ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- flatten lists
concat :: [[a]] -> [a] -- aka, :: [[] a] -> [a]
concat xss = [ x | xs <- xss, x <- xs ]

-- flatten any monad, inc lists (aka, concat)
join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x  <- mx
              return x

-- Monad laws
leftIdentity :: (Eq (m b), Monad m) => (a -> m b) -> a -> Bool
leftIdentity f x = (return x >>= f) == f x -- return is the Identity for >>=

rightIdentity :: (Eq (m a), Monad m) => m a -> Bool
rightIdentity mx = (mx >>= return) == mx -- return is the Identity for >>=

associativity :: (Eq (m c), Monad m) => (a -> m b) -> (b -> m c) -> m a -> Bool
associativity f g mx = lg == rg -- proves that grouping brackets are not required for do notation and >>=
  where lg = (mx >>= f) >>= g
        rg = mx >>= (\x -> f x >>= g)

{-
Effectful programming

a -> Maybe b Exceptions / optionality
a -> [b]     Non-determinism (returns multiple/all possible success values for the input a)
a -> ST b    Internal state manipulation
...
a -> IO b    Input / Output

Why Monads?
1. Supports *pure* programming with effects;
2. Use of monads is *explicit* in types;
3. Can generalise functions to *any* effect;

Further reading:
Monadic Parsing    - Programming Haskell C.13
Monad Transformers - Internet
-}

main = do print $ mapM Just [1,2,3]
          print $ mapM (\x -> Nothing :: Maybe Int) [1,2,3]
          print $ mapM conv "1234"
          print $ mapM conv "12a34"
          print $ join (Just (Just "hello"))
          print $ join (Just (Nothing :: Maybe String))
          print $ join [ "hello", " ", "world" ]
          print $ join [ [1,2], [3,4], [5,6] ]
          print $ leftIdentity Just 1
          print $ rightIdentity (Just 1)
          print $ associativity Just Just (Just 5)
