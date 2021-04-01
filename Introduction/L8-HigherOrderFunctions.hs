module HigherOrderFunctions where

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map1 :: (a-> b) -> [a] -> [b] -- simple definition
map1 f xs = [ f x | x <- xs ]

map2 :: (a-> b) -> [a] -> [b]   -- for proofs by induction
map2 _ []     = []              -- base case
map2 f (x:xs) = f x : map2 f xs -- recusive / inductive case

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p xs = [ x | x <- xs, p x ]

filter2 :: (a -> Bool) -> [a] -> [a] -- predicates are function or properties that return True/False
filter2 _ [] = []
filter2 p (x:xs)
    | p x       = x : filter2 p xs
    | otherwise = filter2 p xs

-- foldr
-- many primative recusive solutions follow this basic pattern:
-- f []     = v         -- some value v
-- f (x:xs) = x ⊕ f xs -- ⊕, circled plus, or plusl means some function.
--                      -- Apply ⊕ to the head of the list, and f to the tail
-- sum []     = 0                  -- v  = 0
-- sum (x:xs) = x + sum xs         -- ⊕ = +
-- sum = foldr (+) 0
-- product []     = 1              -- v  = 1
-- product (x:xs) = x + product xs -- ⊕ = *
-- product = foldr (*) 1
-- and []     = True               -- v  = True
-- and (x:xs) = x + and xs         -- ⊕ = &&
-- and = foldr (&&) True
-- or [] = False                   -- v  = False
-- order (x:xs) = x || or xs       -- ⊕ = ||
-- or = foldr (||) False

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)
-- foldr can be thought non-recurively as a replacing (:) with an infix function f and [] with a value v:
-- folding f and v over the list 1:2:3:[] == (1 `f` (2 `f` (3 `f` v)))
-- sum [1..3]
-- = foldr (+) 0 [1,2,3]
-- = foldr (+) 0 (1:2:3:[])
-- = 1+(2+(3+0))
-- = 6

--length' [] = 0                  -- v = 0
--length' (_:xs) = 1 + length' xs -- ⊕ = (1+)
--length' [1,2,3] = 3
--length' (1:(1:(1:[])))
--1+(1+(1+0))
--3
length' :: [a] -> Int
length' = foldr' (\_ y -> 1 + y) 0

--reverse [1,2,3]
--reverse (1:(2:(3:[])))
--((([] ++ [3]) ++ [2]) ++ [1])
--[3,2,1]
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 = foldr' (\x xs -> xs ++ [x]) []

(<++>) :: [a] -> [a] -> [a] -- alias of ++
xs <++> ys = foldr' (:) ys xs
--[1,2] <++> [3,4,5]
--(1:2:[3,4,5])

(<.>) :: (b -> c) -> (a -> b) -> (a -> c) -- 2 functions as input and returns a function
f <.> g = \x -> f (g x)

odd' :: Int -> Bool
odd' = not <.> even

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [ p x | x <- xs ]

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [ p x | x <- xs ]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x : xs

-- Note: higher order functions that return functions are curried functions
-- e.g., take :: Int -> [a] -> [a]
-- = take :: Int -> ([a] -> [a])

map3 :: (a -> b) -> [a] -> [b]
map3 f ys = foldr (\x xs -> f x : xs) [] ys

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 p ys = foldr (\x xs -> if p x then x : xs else xs) [] ys

main = do
    print $ twice (+1) 0 -- (+1) is a section, a partially applied infix operator
    print $ twice (subtract 1) 0 -- cannot create the section (-1) because that is the literal of minus 1
    print $ map1 (+1) [1, 3, 5, 7] -- mapping (+1) across the items in the list
    print $ map2 (+1) [1, 3, 5, 7] -- mapping (+1) across the items in the list
    print $ filter1 even [1..5]
    print $ filter2 even [1..5]
    print $ foldr' (+) 0 [1..6]
    print $ length' [1..7]
    print $ reverse1 [1..10]
    print $ reverse2 [1..10]
    print $ [1..3] <++> [4,5] <++> [6..10]
    print $ odd' 3
    print $ odd' 4
    print $ all' odd' [1, 3, 5]
    print $ takeWhile' (/= 7) [1,3,5,7,8,9,10]
    print $ dropWhile' (== ' ') "    hello world"
    print $ map (*2) (filter even [1,2,3,4,5,6])
    print $ map3 (*2) <.> filter3 even $ [1,2,3,4,5,6]
