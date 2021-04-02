module TypesAndClasses where
import Prelude hiding (repeat, Tree, False, True, Bool, flip, Maybe, Nothing, Just, add)

-- Declare Types, Define Functions
-- 2 ways to declare new types: Type, Data
-- e.g., type String = [Char] -- String is a synonym for the type [Char]

-- Type Declarations are type synonyms

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x-1, y)

repeat :: Int -> (a -> a) -> (a -> a)
repeat 1 f = f
repeat n f = f . repeat (n-1) f

-- Type declarations can have type parameters
type Pair a = (a, a)

mult :: Pair Int -> Int
mult (m, n) = m*n

copy :: a -> Pair a
copy x = (x, x)

-- Type declarations can be nested
type Trans = Pos -> Pos -- Trans defines a function type that translates a position to another position

-- But type declarations cannot be nested or recursive
-- type Tree = (Int, [Tree]) -- reports "cycle in type synonym" error

-- Data Declarations are completely new types
data Boolean = False | True deriving Show
-- False and True are constructors
-- Type and Constructors names must always begin with upper-case letter

data Answer = Yes | No | Unknown deriving Show

answers :: [Answer]
answers = [ Yes, No, Unknown, Yes ]

-- Data type values can be pattern matched in function definitions
flip :: Answer -> Answer
flip Yes     = No
flip No      = Yes
flip Unknown = Unknown

-- Data declaration constructors can also have parameters
data Shape = Circle Float | Rect Float Float
-- Circle and Rect are functions that construct values of the Shape type
-- ghci:
-- Prelude> :t Circle
-- Circle :: Float -> Shape
-- Prelude> :t Rect
-- Rect :: Float -> Float -> Shape

-- Circle 2.0 is just a value

square :: Float -> Shape
square x = Rect x x

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect a b) = a * b

-- Data declarations can also have parameters
data Maybe a = Just a | Nothing deriving Show

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Data declarations can be recursive / defined in terms of themselves
data Nat = Zero | Succ Nat deriving Show
-- Nat is a new type
-- Constructors:
--   Zero :: Nat
--   Succ :: Nat -> Nat
-- Lambda calculus version of natural numbers, Peano natural numbers

-- some infrastructure functions translating Nats & Ints
nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- Some domain functions operating purely on Nats
repeatNat :: Nat -> (a -> a) -> (a -> a)
repeatNat (Succ Zero) f = f
repeatNat (Succ n) f    = f . repeatNat n f

addNat :: Nat -> Nat -> Nat
addNat Zero n     = n
addNat (Succ m) n = Succ (addNat m n)

-- multiplication is repeated addition
mulNat :: Nat -> Nat -> Nat
mulNat Zero n     = Zero
mulNat (Succ m) n = addNat (mulNat m n) n

-- expression tree using integers, addition, and multiplication
data Expr = Val Int       -- leaf
          | Add Expr Expr -- node
          | Mul Expr Expr -- node
          deriving Show

-- constructor types
-- :t Val
-- Val :: Int -> Expr
-- Add :: Expr -> Expr -> Expr
-- Mul :: Expr -> Expr -> Expr

-- count the number of values that appear in the expression
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

-- calculate the expression
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

putTitle :: String -> IO ()
putTitle s = do
    putStrLn s
    putStrLn (replicate (length s) '-')

main = do
    putTitle "Pos"
    print origin
    print $ left origin
    print $ repeat 5 left origin
    print $ mult (5,4)
    print $ copy 7

    putTitle "Boolean"
    print False
    print True

    putTitle "Answers"
    print answers
    print $ map flip answers

    putTitle "Shapes"
    print $ area (Circle 5)
    print $ area (Rect 5 4)
    print $ area (square 5)

    putTitle "Maybe"
    print $ safediv 5 0
    print $ safediv 5 2
    print $ safehead ([] :: [Int])
    print $ safehead [1..3]

    putTitle "Peano numbers"
    print Zero
    print $ Succ Zero
    print $ Succ (Succ Zero)
    print $ nat2int Zero
    print $ nat2int (Succ Zero)
    print $ nat2int (repeat 5 Succ Zero)
    print $ repeatNat (Succ (Succ Zero)) left origin -- can eventually define left and origin in terms of Nats ;)
    print $ int2nat 5
    print $ nat2int $ addNat Zero Zero
    print $ nat2int $ addNat (Succ (Succ (Succ Zero))) (Succ (Succ (Succ (Succ Zero)))) -- 3 + 4
    print $ nat2int $ mulNat (Succ (Succ (Succ Zero))) (Succ (Succ (Succ (Succ Zero)))) -- 3 * 4
    print $ nat2int $ mulNat (Succ (Succ (Succ Zero))) Zero -- 3 * 0

    putTitle "Expressions"
    print $ Add (Val 1) (Mul (Val 2) (Val 3)) -- + 1 (* 2 3) aka 1 + (2 * 3)
    print $ size $ Add (Val 1) (Mul (Val 2) (Val 3)) -- 3 value terms 1, 2, and 3
    print $ eval $ Add (Val 1) (Mul (Val 2) (Val 3))

    putTitle "Tree"
    print $ Node (Leaf 3) (Leaf 4)
    print $ Node (Node (Leaf "Hello") (Leaf "World")) (Node (Leaf "Good") (Leaf "bye"))

-- Expand
-- add :: Nat -> Nat -> Nat
-- add Zero n     = n
-- add (Succ m) n = Succ (add m n)

-- add (Succ (Succ Zero)) (Succ Zero)
-- = Succ ((add (Succ Zero)) (Succ Zero))
-- = Succ (Succ (add Zero (Succ Zero)))
-- = Succ (Succ (Succ Zero))

-- e.g., add corresponds to laws of arithmetic:
-- 0 + n = n                 -- left identity
-- (1 + m) + n = 1 + (m + n) -- associativity