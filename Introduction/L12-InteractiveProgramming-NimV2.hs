module Nim where

import Data.Char
import Text.Read

-- Implemented by prof for reference...

-- Board utilities:

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move b row num = [adjust r n | (r,n) <- zip [1..5] b]
                 where
                     adjust r n = if r == row then n-num else n

-- I/O utilities:

newline :: IO ()
newline = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)

putBoard :: Board -> IO () -- intentional hard-coded hack
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

safeReadInt :: IO (Maybe Int)
safeReadInt = (\c -> readMaybe c :: Maybe Int) <$> getLine

getDigit :: String -> IO Int -- this function works around a Windows/GHC getChar bug that buffers input. See getDigit'
getDigit prompt = do putStr prompt
                     c <- safeReadInt
                     maybe errorLoop return c -- repeat errorLoop until c == Just Int, then unwrap and return Int
  where
     errorLoop = do newline
                    putStrLn "Error: Invalid digit"
                    getDigit prompt

-- Nim game:
next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player =
  do newline
     putBoard board
     newline
     if finished board then
        do putStr "Player "
           putStr (show (next player)) -- in this case structure, then *other player* won
           putStrLn " wins"
     else
        do putStr "Player "
           print player
           r <- getDigit "Enter a row number: "
           n <- getDigit "Enter stars to remove: "
           if valid board r n then
               play (move board r n) (next player)
           else
               do newline
                  putStrLn "Error: invalid digit, try again"
                  play board player

nim :: IO ()
nim = play initial 1

-- getDigit' :: String -> IO Int -- desired version in GHC 9+
-- getDigit' prompt =
--   do putStr prompt
--      x <- getChar -- this fails on Windows/GHC, buffers until newline, next call to getChar receives the newline.
--      newline
--      if isDigit x then
--          return (digitToInt x)
--      else
--          do newline
--             putStrLn "Error: Invalid digit"
--             getDigit' prompt