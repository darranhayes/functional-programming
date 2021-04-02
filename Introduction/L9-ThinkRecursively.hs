module ThinkRecursively where
import Prelude hiding (sum, drop, init)

-- Systematic 7 step process to find a recursive definition
-- e.g., summation of a list

-- 1 - Name the function. Prefer short concise names.
-- sum

-- 2* - Write a simple type definition of the function.
-- sum :: [Int] -> Int

-- 3* - Enumerate the obvious cases. For a single list argument, there are a minimum of 2 standard cases (empty, non-empty)
--   - May need to be refined later.
-- sum []     =
-- sum (x:xs) =

-- 4 - Define the simple cases
-- sum []     = 0 -- the sum of the empty list is zero
-- sum (x:xs) =

-- 5 - List the actual & potential "ingredients" we can think of
--   - The function 'sum' itself is an ingredient, x and xs are ingredients, constants (e.g., 0, 1, etc.), library functions, etc.
-- sum (x:xs) = -- sum, x, xs, 0, +

-- 6 - Define the other cases as simply as possible
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- 7* - Generalise & Simplify
--   - Think about the solution. What have we got and can it be improved?
--   - Can the solution be generalised? Find the most general type signature possible.
--   - Can the definition be simplified? Switch from primative recusions to foldr.
--   - Can guards be replaced with pattern matching?
--   - Use wildcards for unused arguments
sum :: Num a => [a] -> a
sum = foldr (+) 0

-- e.g., drop a given number of elements from the start of a list
-- 1 - Name
-- drop

-- 2 - Type signature
-- drop :: Int -> [a] -> [a]

-- 3 - Simple cases
-- drop 0 []
-- drop 0 (x:xs)
-- drop n []
-- drop n (x:xs)

-- 4 - Simple case definitions
-- drop 0 []     = []
-- drop 0 (x:xs) = x:xs
-- drop n []     = []
-- drop n (x:xs) =

-- 5 - Ingredients
-- drop n (x:xs) = -- drop, :, [], -, library functions for lists & integers

-- 6 - Define other cases
-- drop 0 []     = []
-- drop 0 (x:xs) = x:xs
-- drop n []     = [] -- could return an error, or return empty list
-- drop n (x:xs) = drop (n-1) xs -- "Q: how to drop e.g., 5 elements from the list x:xs? A: By dropping 4 elements from the list xs"

-- 7 - Generalise, simplify, refactor
drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

-- e.g., remove the last element from non-empty list
-- 1 - Name
-- init -- e.g., "initial segment"

-- 2 - Type signature
-- init :: [a] -> [a] -- note: this is technically an empty or non-empty list, so we're a little weak here

-- 3 - Simple cases
-- init (x:xs) -- don't need to consider empty lists

-- 4 - Simple case definitions
-- init (x:xs) | null xs   = [] -- if the remaining part of the list is empty, then we can discard x as it's the last item
--             | otherwise =

-- 5 - Ingredients
-- init (x:xs) = -- init, :, [], x, xs, library functions for lists

-- 6 - define other cases
-- init (x:xs) | null xs   = []
--             | otherwise = x : init xs

-- 7 - Generalise, refactor, pattern match, wildcard unused arguments
init :: [a] -> [a]
init [_]    = []
init (x:xs) = x : init xs

main = do
    print $ sum [1..10]
    print $ drop 5 [1..10]
    print $ init [1..10]