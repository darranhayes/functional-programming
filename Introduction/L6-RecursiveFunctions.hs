module RecursiveFunctions where

fac :: Int -> Int
fac n = product [1..n]

-- recursive definition of product
product' :: Num a => [a] -> a
product' []     = 1 -- base case, and 1 is the identity value for multiplication
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0 -- base case, and 0 is the identity value for addition
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n-1) xs

and' :: [Bool] -> Bool -- also, and = foldr (&&) True
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a] -- also, foldr (++) []
concat' []     = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _  = []
replicate' n x = x: replicate (n-1) x

(!!!) :: [a] -> Int -> a
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)

quicksort' :: Ord a => [a] -> [a]
quicksort' []     = []
quicksort' (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [ a | a <- xs, a <= x ]
    larger  = [ b | b <- xs, b > x ]

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | y >= x    = x : y : ys
                | otherwise = y : insert x ys

insertsort :: Ord a => [a] -> [a]
insertsort []     = []
insertsort (x:xs) = insert x (insertsort xs) -- also, foldr insert []

merge :: Ord a => [a] -> [a] -> [a]
merge []     ys                     = ys
merge xs     []                     = xs
merge a@(x:xs) b@(y:ys) | x <= y    = x : merge xs b
                        | otherwise = y : merge a ys

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort lhs) (mergesort rhs)
  where
    (lhs, rhs) = (take n xs, drop n xs) -- also, splitAt (length xs `div` 2) xs
    n          = length xs `div` 2

main = do
    print $ fac 10
    print $ product' [1..10]
    print $ length' [1..10]
    print $ reverse' [1..10]
    print $ zip' [1..] ['a'..'z']
    print $ drop' 5 [1..10]
    print $ and' [True, True, True, True]
    print $ and' [True, True, False, True]
    print $ concat' [[1,2], [], [3], [4,5,6]]
    print $ replicate' 5 'a'
    print $ "hello world" !!! 6
    print $ elem' 'w' "hello world"
    print $ elem' 'z' "hello world"
    print $ quicksort [7, 3, 4, 1, 9, 9, 3, 7, 4, 5, 10]
    print $ quicksort' [7, 3, 4, 1, 9, 9, 3, 7, 4, 5, 10]
    print $ insert 3 [1,2,4,5]
    print $ insert 3 [1,2,3,4,5]
    print $ insertsort [7, 3, 4, 1, 9, 9, 3, 7, 4, 5, 10]
    print $ merge [1, 4, 7] [2, 3, 5, 6]
    print $ mergesort [7, 3, 4, 1, 9, 9, 3, 7, 4, 5, 10]

-- expansion of quicksort' [7, 3, 4, 1, 9, 9, 3, 7, 4, 5, 10]
-- quicksort' []     = []
-- quicksort' (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
--   where
--     smaller = [ a | a <- xs, a <= x ]
--     larger  = [ b | b <- xs, b > x ]
--
-- quicksort' [7, 3, 4, 1, 9, 9, 3, 7, 4, 5, 10]
-- = quicksort' [3, 4, 1, 3, 7, 4, 5 ] ++ [7] ++ quicksort' [9, 9, 10]
-- = quicksort' [1, 3] ++ [3] ++ quicksort' [4, 7, 4, 5] ++ [7] ++ quicksort' [9] ++ [9] ++ quicksort' [10]
-- = quicksort' [] ++ [1] ++ quicksort' [3] ++ [3] ++ quicksort' [4] ++ [4] ++ quicksort' [7, 5] ++ [7] ++ [9] ++ [9] ++ [10]
-- = [] ++ [1] ++ [3] ++ [3] ++ [4] ++ [4] ++ quicksort' [5] ++ [7] ++ quicksort' [] ++ [7] ++ [9] ++ [9] ++ [10]
-- = [] ++ [1] ++ [3] ++ [3] ++ [4] ++ [4] ++ [5] ++ [7] ++ [] ++ [7] ++ [9] ++ [9] ++ [10]
-- = [1, 3, 3, 4, 4, 5, 7, 7, 9, 9, 10]