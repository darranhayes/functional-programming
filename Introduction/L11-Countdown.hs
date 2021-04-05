module Countdown where

import Data.List

-- C4 Countdown game
-- given a set of random numbers, e.g., 1 3 7 10 25 50
-- with the 4 basic arithmetic operators, + - * / (whole numbered divide)
-- construct an expression whose value is, e.g., 765

-- rules
-- all numbers and intermediate results are positive natural numbers (no negatives, no zero, no proper fractions e.g., 2/4, 1/3, etc.)
-- source numbers can be used only once

data Op = Add | Sub | Mul | Div deriving Show

ops :: [ Op ]
ops = [ Add, Sub, Mul, Div ]

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- original with inefficiencies
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- eliminates otherwise valid possibilities by exploiting laws of commutativity & arithmetic identities
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = x `mod` y == 0 && y /= 1

data Expr = Val Int | App Op Expr Expr deriving Show

-- eval succeeds with a singleton list, or fails with empty list
eval :: Expr -> [Int]
eval (Val n)     = [ n | n > 0 ] -- base case
eval (App o l r) = [ apply o x y | x <- eval l
                                 , y <- eval r
                                 , valid o x y ] -- recursive case

deleteAt :: Int -> [a] -> [a]
deleteAt i ts = take i ts ++ drop (i+1) ts

-- permutations
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms ps = [ x:xs | (x, i) <- zip ps [0..]
                  , xs     <- perms (deleteAt i ps) ]

-- subsequences
subs :: Eq a => [a] -> [[a]]
subs [] = [[]]
subs ls = nub $ [] : ls : [ xs | i  <- [0..length ls -1]
                               , xs <- subs (deleteAt i ls) ]

-- unused: combinations
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys    <- combinations (n-1) xs' ]

-- choices [1,2] = [[], [1], [2], [1,2], [2,1]]
choices :: Eq a => [a] -> [[a]]
choices = nub . concatMap subsequences . permutations -- faster using library functions
--choices = nub . concatMap subs . perms

sortedChoices :: (Ord a) => [a] -> [[a]]
sortedChoices = sortBy (\x y -> compare (length x, x) (length y, y)) . choices

-- list of values used in an expression
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- decision function: is the expression a solution for a list of source numbers and a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- find all ways to partition a list
split :: [a] -> [([a],[a])]
split xs = [ splitAt n xs | n <- [1..len] ]
  where
    len = length xs - 1

combine :: Expr -> Expr -> [Expr]
combine l r = [ App o l r | o <- ops ]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [ e | (ls, rs) <- split ns
                , l        <- exprs ls -- generate all possible left side expressions
                , r        <- exprs rs -- generate all possible right side expressions
                , e        <- combine l r ]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [ e | ns' <- choices ns
                     , e   <- exprs ns'
                     , eval e == [n] ]

-- above approach is very slow as evaluation follows generation serially,
-- try and fuse generation and evaluation step with, e.g., this type:
type Result = (Expr, Int)

-- slow but useful specification of the fusion of generation and evaluation
-- results :: [Int] -> [Result]
-- results ns = [ (e,n) | e <- exprs ns
--                      , n <- eval e ]

-- my attempt #1
combine1 :: Result -> Result -> [Result]
combine1 (l, x) (r, y) = [ (e, n) | o <- ops
                                  , valid o x y
                                  , e <- [ App o l r ] -- urgh
                                  , n <- eval e -- should've considered apply & a valid guard instead
                                  ]

results1 :: [Int] -> [Result]
results1 []  = []
results1 [n] = [ (Val n, n) | n > 0 ]
results1 ns  = [ res | (ls, rs) <- split ns
                     , lx       <- results1 ls
                     , ry       <- results1 rs
                     , res      <- combine1 lx ry ]

solutions1 :: [Int] -> Int -> [Expr]
solutions1 ns n = [ e | ns'   <- choices ns
                      , (e,m) <- results1 ns'
                      , m == n ]

-- prof's solution (also with faster valid' fn):
combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) = [ (App o l r, apply o x y) | o <- ops
                                                   , valid' o x y ] -- filters any partially invalid expressions

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [ (Val n, n) | n > 0 ]
results' ns  = [ res | (ls, rs) <- split ns
                     , lx       <- results' ls
                     , ry       <- results' rs
                     , res      <- combine' lx ry ]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [ e | ns'    <- choices ns
                      , (e, m) <- results' ns'
                      , m == n ]

main = do
    print $ apply Add 5 7
    print $ apply Div 6 3
    print $ valid Div 6 3
    print $ valid Div 3 6
    print ops
    let e = App Mul (App Sub (Val 25) (Val 10)) (App Add (Val 50) (Val 1))
    let ns = [1,3,7,10,25,50]
    print $ values e
    print $ take 50 $ choices ns
    print $ eval e
    print $ solution e ns 765
    print $ take 1 $ solutions [1,3,7,10,25,50] 765
    print $ take 1 $ solutions' [1,3,7,10,25,50] 765
    print $ length $ solutions' [1,3,7,10,25,50] 765