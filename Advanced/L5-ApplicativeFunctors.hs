module ApplicativeFunctors where

import Prelude hiding (Functor, fmap, Maybe, Just, Nothing, pure, Applicative, pure, (<*>))
import Data.Char

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = map

data Maybe a = Nothing | Just a
               deriving Show

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

-- Functor/fmap is only defined for unary functions of (a -> b)
-- So, fmap (+) (Just 1) (Just 2), is not valid

-- We could tediously specify variants of fmap as follows:
-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b -- fmap
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

class Functor0 f where
    fmap0 :: a -> f a

instance Functor0 Maybe where
    -- fmap0 :: a -> f a
    fmap0 x = Just x

class Functor1 f where
    fmap1 :: (a -> b) -> f a -> f b

instance Functor1 Maybe where
    -- fmap1 :: (a -> b) -> f a -> f b
    fmap1 = fmap

class Functor2 f where
    fmap2 :: (a -> b -> c) -> f a -> f b -> f c

instance Functor2 Maybe where
    -- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
    fmap2 f Nothing _         = Nothing
    fmap2 f _ Nothing         = Nothing
    fmap2 f (Just a) (Just b) = Just (f a b)

-- Instead, let's generalise Functor/fmap to functions with any number of arguments, called Applicative
class Applicative f where
    pure  :: a -> f a
    (<*>) :: f(a -> b) -> f a -> f b -- data structure that takes a function, and a data structure, returns a data structure

-- "Applicative style for Maybe supports a form of exceptional programming where we can apply pure functions
-- to arguments that may fail"
instance Applicative Maybe where
    -- pure  :: a -> f a
    pure x = Just x
    -- (<*>) :: f(a -> b) -> f a -> f b
    (<*>) Nothing _ = Nothing
    (<*>) (Just f) Nothing = Nothing
    (<*>) (Just f) (Just x) = Just (f x)

adder :: Int -> Int -> Int -> Int
adder x y z = x + y + z

f0 :: Maybe (Int -> Int -> Int -> Int)
f0 = pure adder
f1 :: Maybe (Int -> Int -> Int)
f1 = pure adder <*> Just 1
f2 :: Maybe (Int -> Int)
f2 = pure adder <*> Just 1 <*> Just 5
f3 :: Maybe Int
f3 = pure adder <*> Just 1 <*> Just 5 <*> Just 6 -- <*> associates to the left as per regular function application:
f3' :: Maybe Int
f3' = ((pure adder <*> Just 1) <*> Just 5) <*> Just 6

-- "Applicative style for Lists supports a form of non-deterministic programming where we apply pure functions
-- to multi-valued arguments"
instance Applicative [] where
    -- pure  :: a -> f a
    pure x = [x]
    -- (<*>) :: f(a -> b) -> f a -> f b
    gs <*> ys = [ g y | g <- gs, y <- ys ]

pair x y = (x,y)
pairList x y = [x,y]

main = do print (fmap0 5 :: Maybe Int)
          print $ fmap2 (+) (Just 1) (Just 2)
          -- "Applicative style" because it's very similar to normal function application: g x y
          print $ pure (+) <*> Just 3 <*>  Just 4
          print $ pure not <*> Just True
          print $ pure (+1) <*> [1,2,3] -- [2,3,4]
          print $ pure length <*> ["hello", "world"] -- [5,5]
          print $ [ minimum . map ord, maximum . map ord ] <*> ["hello", "world"]
          print $ pure (+) <*> [1, 2] <*> [3, 4] -- [1+3, 1+4, 2+3, 2+4]
          print $ pure (*) <*> [1, 2, 3] <*> [4, 5, 6] -- [1*4, 1*5, 1*6, 2*4, 2*5, 2*6, 3*4, 3*5, 3*6]
          print $ pure pair <*> [1, 2] <*> [3, 4] -- [(1,3), (2,3), (2,3), (2,4)]
          print $ pure pairList <*> [1, 2] <*> [3, 4] -- [[1,3], [1,4], [2,3], [2,4]]
