module ListComprehensions where

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x ]

allprimes :: [Int]
allprimes = sieve [2..]

sieve :: [Int] -> [Int] -- sieve of eratosthenes
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [ x <= y | (x,y) <- pairs xs ] -- or sorted = all (uncurry (<=)) . pairs

positions :: Ord a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0..], x' == x ]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n],
                        y <- [1..n],
                        z <- [1..n],
                        x^2 + y^2 == z^2 ]

perfect :: Int -> Bool
perfect n = sum (init $ factors n) == n

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], perfect x ]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x*y | (x, y) <- zip xs ys ]

main = do
    print $ factors 10
    print $ "Is 12 prime: " ++ show (prime 12)
    print $ "Is 13 prime: " ++ show (prime 13)
    print $ primes 100
    print $ takeWhile (<= 100) allprimes
    print $ sorted $ [1..4] ++ [9] ++ [15..20]
    print $ sorted $ [1..4] ++ [3] ++ [15..20]
    print $ positions 3 ([1..5] ++ [1..5])
    print $ pyths 10
    print $ perfects 500
    print $ scalarproduct [3, 4, 5] [3, 4, 5]

-- example example of sieve [2..15]
-- sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]
-- sieve [2..15] = 2 : sieve [ 3, 5, 7, 9, 11, 13, 15 ] -- divisible by 2
--               = 2 : 3 : sieve [ 5, 7, 11, 13] -- divisible by 3
--               = 2 : 3 : 5 : sieve [ 7, 11, 13 ] -- divisible by 5
--               = 2 : 3 : 5 : 7 : sieve [ 11, 13 ] -- divisible by 7
--               = 2 : 3 : 5 : 7 : 11 : sieve [13] -- divisible by 11
--               = 2 : 3 : 5 : 7 : 11 : 13 : sieve []
--               = 2 : 3 : 5 : 7 : 11 : 13 : []
--               = [2, 3, 5, 7, 11, 13]