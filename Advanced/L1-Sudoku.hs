-- Sudoku solver
-- http://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs

import Data.List

-- Basic declarations
type Grid = Matrix Value
type Matrix a = [Row a] -- list of lists, 2D. Paramterised for future flexibility
type Row a = [a]
type Value = Char
type Choices = [Value]

boxsize :: Int
boxsize = 3

easy :: Grid -- solved by solve3
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

gentle :: Grid
gentle =  [".1.42...5",
           "..2.71.39",
           ".......4.",
           "2.71....6",
           "....4....",
           "6....74.3",
           ".7.......",
           "12.73.5..",
           "3...82.7."]

diabolical :: Grid
diabolical =  [".9.7..86.",
               ".31..5.2.",
               "8.6......",
               "..7.5...6",
               "...3.7...",
               "5...1.7..",
               "......1.9",
               ".2.6..35.",
               ".54..8.7."]

minimal :: Grid
minimal =  [".98......",
            "....7....",
            "....15...",
            "1........",
            "...2....9",
            "...9.6.82",
            ".......3.",
            "5.1......",
            "...4...2."]

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

singles :: [[a]] -> [a]
singles xss = concat (filter single xss)

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

-- cartesian product
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ y:ys | y <- xs, ys <- cp xss ] -- cp (xs:xss) = xs >>= \x -> cp xss >>= \y -> [x : y]

-- the fixpoint of a function is an input value applied to the function that results in the same value
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
          where x' = f x

-- no duplicates in any row, column, or box (3*3)
valid :: Grid -> Bool
valid g = all nodups (rows g) && --
          all nodups (cols g) &&
          all nodups (boxs g)

-- considers all possible combinations for each cell
explode :: Matrix [a] -> [Matrix a]
explode = cp . map cp

-- takes a grid, replaces blank cell with all possible choices for that cell
choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v = if v == '.' then ['1'..'9'] else [v]

-- reduce the number of choices in a cell that are present in single choices in that row, column, or box
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows -- boxs . boxs = id, cols . cols = id, rows . rows = id
        where pruneBy f = f . map reduce . f

-- e.g., reduce ["1234", "1", "34", "3"], note "1", and "3", are determined values, so we can remove 1 and 3 from "1234", and "34"
reduce :: Row Choices -> Row Choices
reduce xss =  [ xs `minus` singles xss | xs <- xss ]

minus :: Choices -> Choices -> Choices
minus xs ys = if single xs then xs else xs \\ ys

-- Naive: given the easy grid, produces 9^51 (9 digits raised to 51 empty squares to explore) grids to validate.
solve1 :: Grid -> [Grid]
solve1 = filter valid . explode . choices

-- approx 10^24 earch space
solve2 :: Grid -> [Grid]
solve2 = filter valid . explode . prune . choices

-- repeatedly prune output from previous pruning until no improvements
solve3 :: Grid -> [Grid]
solve3 = filter valid . explode . fix prune . choices

-- Blocked matrices -> cannot get a solution from these -> but collapse will create lots of candidates from a blocked matrix.

-- A void choice matrix ["1", ".", "23"], then the 2nd cell is blocked or 'void'. "At least one cell has no choices"
void :: Matrix Choices -> Bool
void = any (any null) -- any row has any empty cell

-- A safe matrix has consistent rows, columns, and boxes. Consistent -> *no duplicate single entries*.
-- ["1", "23", "1", "4"] is inconsistent
-- ["1234", "1", "34", "4"] is consistent
consistent :: Row Choices -> Bool
consistent = nodups . singles

safe :: Matrix Choices -> Bool
safe m = all consistent (rows m) &&
         all consistent (cols m) &&
         all consistent (boxs m)

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m) -- there are empty cells (no choices) or there are duplicated single cells

-- similar to explode, but only for the first cell of more than one choices
expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
           where
             (rows1, row:rows2) = span (all single) m -- break and span partition a list based on a predictate
             (row1, cs:row2)    = span single row

search :: Matrix Choices -> [Grid]
search m | blocked m          = []
         | all (all single) m = explode m
         | otherwise          = [ g | m' <- expand m,
                                      g  <- search (prune m') ]

solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

pretty :: Grid -> Grid -> IO ()
pretty p s = let pairs = zip p s
              in putStrLn . unlines $ map formatPair pairs
             where formatPair (x, y) = x ++ "   " ++ y

solve :: String -> Grid -> IO ()
solve t g = do putStrLn $ t ++ " (" ++ show (length slns) ++ " - max 1000)"
               pretty g (head slns)
               where slns = take 1000 . solve4 $ g

main = do solve "Easy" easy
          solve "Gentle" gentle
          solve "Minimal" minimal
          solve "Diabolical" diabolical
