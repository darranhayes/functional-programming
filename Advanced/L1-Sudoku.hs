module Sudoku where

-- Note: many properties look trivial, but will be later in reasoning about the logic

import Data.List

-- Basic declarations
type Grid = Matrix Value
type Matrix a = [Row a] -- list of lists, 2D. Paramterised for future flexibility
type Row a = [a]
type Value = Char

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

valid :: Grid -> Bool
valid g = all nodups (rows g) && --
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

main = do print True