module Monads where

import Prelude hiding (Monad, (>>=), pure)

-- Example: an evaluator

data Expr = Val Int | Div Expr Expr
            deriving Show

e = Div (Val 6) (Val 3) -- == 6 /3

-- fails for division by zero
eval :: Expr -> Int
eval (Val x)   = x
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

-- cannot implement "eval2 x `safediv` eval2 y" as safediv takes Int, rather than Maybe Int returned by nest calls to eval2;
-- need to pattern match on the return values
eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Div x y) = case eval2 x of
                    Nothing -> Nothing
                    Just m  -> case eval2 y of
                                 Nothing -> Nothing
                                 Just n  -> m `safediv` n -- we have valid Int arguments here, we can divide using a maybe returning function

-- Failed attempt at applicative style
-- evalA :: Expr -> Maybe Int
-- evalA (Val x)   = pure x
-- evalA (Div x y) = pure safediv <*> evalA x <*> evalA y -- uncomment for type mistmatch

-- Return type for the 2nd case for of evalA is: Maybe (Maybe Int); doesn't match the signature
-- Given safediv :: Int -> Int -> Maybe Int
-- Then pure safediv :: Maybe(Int -> Int -> Maybe Int)
-- evalA x :: Maybe Int
-- evalA y :: Maybe Int
-- Need safediv to be Int -> Int -> Int and use applicatives to apply the Maybe args.
-- Safediv is *not pure* as it's Int -> Int -> Maybe Int.
-- Applicative style is "pure functions" applied to effectful arguments.

-- instead, try and factor out and paramterise the case statements over arguments x y and function f
doer :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
doer f mx my = case mx of -- case of 1st Maybe arg
                 Nothing -> Nothing
                 Just n  -> case my of -- case of 2nd Maybe arg
                              Nothing -> Nothing
                              Just m  -> f n m -- apply f

evalD :: Expr -> Maybe Int
evalD (Val x) = Just x
evalD (Div x y) = doer safediv (evalD x) (evalD y)

-- refine to single case of the single Maybe arg, discovers bind:
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind mx f = case mx of
              Nothing -> Nothing
              Just x  -> f x

evalB :: Expr -> Maybe Int
evalB (Val x) = Just x
evalB (Div x y) = bind (evalB x) (\m -> bind (evalB y) (\n -> safediv m n))

-- refinement with >>=, infix version of bind
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
             Nothing -> Nothing
             Just x  -> f x

evalM :: Expr -> Maybe Int
evalM (Val x) = Just x
evalM (Div x y) = evalB x >>= \m -> -- evaluate x, if successful, then with the result a
                  evalB y >>= \n -> -- evaluate y, if successful, then with the result b
                  safediv m n       -- perform the safediv with a and b

-- if any mx fails (is == Nothing), then the overall expression will fail (with Nothing)
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
--     .
--     .
--     .
-- mn >>= \xn ->
-- f x1 x2 ... xn

-- the above workflow is so common that Haskell provides syntactic sugar with do notation:
-- do x1 <- m1
--    x2 <- m2
--    .
--    .
--    .
--    xn <- mn
--    f x1 x2 ... xn

-- this definition is as simple as the unsafe version of eval
-- this definition appears to be very imperative, familiar from other languages
-- but it is still functional
evalDo :: Expr -> Maybe Int
evalDo (Val x)   = Just x
evalDo (Div x y) = do m <- evalDo x
                      n <- evalDo y
                      safediv m n

evaluator f = do print $ f $ Div (Val 6) (Val 3)
                 print $ f $ Div (Val 6) (Val 0) -- safe divide by zero = Nothing
                 print $ f $ Div (Div (Val 144) (Val 12)) (Val 3)
                 print $ f $ Div (Div (Val 144) (Val 0)) (Val 3) -- safe divide by zero = Nothing


main = do pretty "Unsafe division"
          print $ eval $ Div (Val 6) (Val 3)
          --print $ eval $ Div (Val 6) (Val 0) -- exception
          pretty "Safe division with boilerplate cases"
          evaluator eval2
          pretty "doer"
          evaluator evalD
          pretty "bind"
          evaluator evalB
          pretty ">>="
          evaluator evalM
          pretty "do notation"
          evaluator evalDo

pretty :: String -> IO ()
pretty t = do putStrLn ""
              putStrLn t
              putStrLn $ replicate (length t) '-'
