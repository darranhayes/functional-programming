module Functors where

import Prelude hiding (map, Functor, fmap, Maybe, Just, Nothing)

-- pure   = programs are mathematical *functions* (no interaction, no side-effects)
-- impure = programs with *side-effects* (input, output, state, etc.)

-- Q: is it possible to combine the two approaches?
-- A: Yes, with monads

-- Abstracting programming patterns

increment :: [Int] -> [Int]
increment [] = []
increment (n:ns) = n+1 : inc ns

square :: [Int] -> [Int]
square [] = []
square (n:ns) = n*n : square ns

-- in both above cases, the same pattern emerges, everything is the same except for the operation performed on n.
-- The pattern: doing something to every element of the list:

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- we've abstracted the operation out to f and encapsulated the common behaviour of applying f to every item in the list.

inc = map (+1)
sqr = map (^2)

x :: [] a -> [] a
x a = a

-- Generalising futher, we can map over other structures than just lists.
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-- a parameterised type f is a member of the functor class if it has this fmap function defined

instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = map

-- [] is the paramterised type of list without a (type) argument, in addition to the empty list ;)
-- note type declaration [a] is the same as [] a

data Maybe a = Nothing | Just a
               deriving Show

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x)   = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- Why use functors?
-- Same name, fmap, for functions that are essentially the same (instead of mapMaybe, mapList, mapTree);
-- Can define generic functions that work for any Functorial type (e.g., Functor is the parameter type);

inc' :: Functor f => f Int -> f Int -- this gives us an inc for *any functor* type
inc' = fmap (+1)

main = do print $ fmap (+1) Nothing
          print $ fmap (*3) (Just 4)
          print $ fmap even (Node (Leaf 5) (Node (Leaf 4) (Leaf 9)))
          print $ fmap length (Leaf "hello")
          print $ fmap length (Node (Leaf "hello") (Leaf "world"))
          print $ inc' (Just 4)
          print $ inc' (Node (Leaf 5) (Node (Leaf 4) (Leaf 9)))