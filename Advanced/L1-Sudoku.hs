module Sudoku where

-- Note: many properties look trivial, but will be later in reasoning about the logic

import Data.List

-- Basic declarations
type Grid = Matrix Value
type Matrix a = [Row a] -- list of lists, 2D. Paramterised for future flexibility
type Row a = [a]
type Value = Char
type Choices = [Value]

boxsize :: Int
boxsize = 3

easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."]

solved :: Grid
solved = ["435269781",
          "682571493",
          "197834562",
          "826195347",
          "374682915",
          "951743628",
          "519326874",
          "248957136",
          "763418259"]

blank :: Grid
blank = replicate n (replicate n '.')
  where n = boxsize ^ 2

rows :: Matrix a -> [Row a]
rows = id -- or, rows m = m
          -- Property: rows . rows = id

cols :: Matrix a -> [Row a]
cols = transpose -- Flip matrix along the diagonal (aka, Matrix Transposition)
                 -- Property: cols . cols = id

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack -- map each 3*3 square and map to a single row
       where                    -- Property: boxs . boxs = id, concat . split = id
          pack   = split . map split
          split  = chop boxsize
          unpack = map concat . concat

chop      :: Int -> [a] -> [[a]]
chop n [] =  []
chop n xs =  take n xs : chop n (drop n xs)

single :: [a] -> Bool
single [_] = True
single _   = False

valid :: Grid -> Bool
valid g = all nodups (rows g) && --
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

cp :: [[a]] -> [[a]] -- cartesian product
cp []       = [[]]
cp (xs:xss) = [ y:ys | y <- xs, ys <- cp xss ] -- cp (xs:xss) = xs >>= \x -> cp xss >>= \y -> [x : y]

explode :: Matrix [a] -> [Matrix a]
explode = cp . map cp

choices :: Grid -> Matrix Choices
choices = map (map choice)
          where
            choice v = if v == '.' then
                         ['1'..'9']
                       else
                         [v]

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows -- boxs . boxs = id, cols . cols = id, rows . rows = id
        where pruneBy f = f . map reduce . f

-- e.g., reduce ["1234", "1", "34", "3"], note "1", and "3", are singular choices, so we can remove 1 and 3 from "1234", and "34"
reduce :: Row Choices -> Row Choices
reduce xss =  [ xs `minus` singles | xs <- xss ]
              where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
minus xs ys = if single xs then xs else xs \\ ys

-- Naive: given the easy grid, produces 9^51 (9 digits raised to 51 empty squares to explore) grids to validate.
solve :: Grid -> [Grid]
solve = filter valid . explode . choices

-- approx 10^24 earch space
solve2 :: Grid -> [Grid]
solve2 = filter valid . explode . prune . choices

main = do solve2 easy