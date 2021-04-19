{-# LANGUAGE DeriveFunctor #-}
module TypesAndClasses3 where

{-
Exploring catamorphisms on Tree structures based on recursion schemes & algebra talks and articles:
https://github.com/haroldcarr/presentations/blob/master/2017-05-27-lambdaconf-recursion-schemes.pdf
https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
https://medium.com/@jnkrtech/catamorphisms-8637f3d45383
https://www.youtube.com/watch?v=F5NjINDNwYc
-}

-- Utility functions (also defined in Data.Fix module)
-- Fix :: (* -> *) -> *
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

{-
A catamorphism is a function that “tears down” a data structure by recursing on it with a function
that collapses a functor into its contained type. cata takes such a function, and returns a catamorphism.
-}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Expression Tree encoding raw values, addition, and multiplication
-- ExprF :: * -> * -> *
data ExprF a r = Val a
               | Add r r
               | Mul r r
               deriving (Functor)

-- Expr :: * -> *
type Expr a = Fix (ExprF a)

-- smart constructors used to build expression trees later
val :: a -> Expr a
val x = Fix (Val x)

add, mul :: Expr a -> Expr a -> Expr a
add l r = Fix (Add l r)
mul l r = Fix (Mul l r)

-- catamorphisms: folding functions over the expression tree
eval :: Num a => Expr a -> a
eval = cata alg
  where
    alg (Val x)   = x
    alg (Add l r) = l + r
    alg (Mul l r) = l * r

size :: Num a => Expr a -> a
size = cata alg
  where
    alg (Val x)   = 1
    alg (Add l r) = l + r
    alg (Mul l r) = l + r

depth :: (Num a, Ord a) => Expr a -> a
depth = cata alg
  where
    alg (Val x)   = 1
    alg (Add l r) = max (l+1) (r+1)
    alg (Mul l r) = max (l+1) (r+1)

maxVal :: (Num a, Ord a) => Expr a -> a
maxVal = cata alg
  where
    alg (Val x) = x
    alg (Add l r) = max l r
    alg (Mul l r) = max l r

minVal :: (Num a, Ord a) => Expr a -> a
minVal = cata alg
  where
    alg (Val x) = x
    alg (Add l r) = min l r
    alg (Mul l r) = min l r

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

pretty :: (Show a) => Expr a -> String
pretty = trim . cata alg
  where
    alg (Val x)   = show x
    alg (Add l r) = show' l "+" r
    alg (Mul l r) = show' l "*" r
    show' l m r   = "(" <> l <> m <> r <> ")"

deepest :: (Show a, Ord a, Num a) => Expr a -> (a, String)
deepest = cata alg
  where
    alg (Val x)   = base x
    alg (Add l r) = handle "add" l r
    alg (Mul l r) = handle "mul" l r
    handle op (ld, le) (rd, re)
        | ld > rd   = leftDeepestPath
        | ld == rd  = balancedPath
        | otherwise = rightDeepestPath
                      where leftDeepestPath  = (ld+1, op <> "(" <> le <> ",_)")
                            balancedPath     = (ld+1, op <> "(" <> le <> "," <> re <> ")")
                            rightDeepestPath = (rd+1, op <> "(_," <> re <> ")")
    base x = (1, show x)

printExpr :: (Show a, Num a, Ord a) => Expr a -> IO ()
printExpr e =
  do putStrLn $ "Expr:      " <> pretty e
     putStr "Eval:      "
     print $ eval e
     putStr "Size:      "
     print $ size e
     putStr "Max Value: "
     print $ maxVal e
     putStr "Min Value: "
     print $ minVal e
     putStr "Depth:     "
     print $ depth e
     putStr "Deepest:   "
     print $ deepest e
     putStrLn ""


main = do
    printExpr $ val 1 `add` (val 2 `mul` val 3)
    printExpr $ (val 3 `add` val 1) `add` (val 1 `add` val 2) `mul` val 5
    printExpr $ (val 3 `add` val 1) `add` (val 2 `mul` (val 2 `add` val 5))
    printExpr $ add (add (val 3) (val 1)) (mul (add (mul (val 3) (val 7)) (mul (val 2) (val 8))) (val 5))

