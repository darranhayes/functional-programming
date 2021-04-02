module TypesAndClasses where

-- expression tree using integers, addition, and multiplication, parameterised for any type
data Expr a = Val a                  -- leaf
             | Add (Expr a) (Expr a) -- node
             | Mul (Expr a) (Expr a) -- node

-- generalised for the widest range of types
folde :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr a -> b
folde ve ae me (Val x) = ve x -- base case, unwrap val
folde ve ae me (Add x y) = ae (folde ve ae me x) (folde ve ae me y) -- recursive handling of add expressions
folde ve ae me (Mul x y) = me (folde ve ae me x) (folde ve ae me y) -- recursive handling of mul expressions

eval :: Num a => Expr a -> a
eval = folde id (+) (*) -- evaluate terms naturally

size :: Num a => Expr a -> a
size = folde (const 1) (+) (+)

depth :: (Num a, Ord a) => Expr a -> a
depth = folde (const 1) subExpr subExpr
  where
    subExpr l r = max (l+1) (r+1)

pretty :: (Show a, Num a, Ord a) => Expr a -> String
pretty = folde show (print "+") (print "*")
  where
    print op l r = "(" <> l <> op <> r <> ")"

deepest :: (Show a, Ord a, Num a) => Expr a -> (Int, String)
deepest = folde (\x -> (1, show x)) (handleExpr "add") (handleExpr "mul")
  where
    handleExpr op (ld, le) (rd, re)
        | ld > rd   = (inc ld, op <> "(" <> le <> ",_)")
        | ld == rd  = (inc ld, op <> "(" <> le <> "," <> re <> ")")
        | otherwise = (inc rd, op <> "(_," <> re <> ")")
    inc = (+1)

main = do
    print $ size $ Val 1 `Add` (Val 2 `Mul` Val 3)
    print $ eval $ Val 1 `Add` (Val 2 `Mul` Val 3)
    print $ depth $ (Val 3 `Add` Val 1) `Add` (Val 1 `Add` Val 2) `Mul` Val 5
    putStrLn $ pretty $ (Val 3 `Add` Val 1) `Add` (Val 1 `Add` Val 2) `Mul` Val 5
    print $ deepest $ Add (Add (Val 3) (Val 1)) (Mul (Add (Mul (Val 3) (Val 7)) (Mul (Val 2) (Val 8))) (Val 5))

