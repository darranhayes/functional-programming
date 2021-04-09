module Nim where

import Data.List
import Data.Functor
import Data.Foldable
import System.IO

-- Nim is a board game
-- 5 rows of stars:
--
-- 1: * * * * *
-- 2: * * * *
-- 3: * * *
-- 4: * *
-- 5: *

-- two players, take turns to remove one or more stars from the end of a single row
-- the winner is the player who removes the last star or stars from the board
-- hint: represent the board as a list of five integers that indicate remaining stars on the row
--       initial board is: [5,4,3,2,1]

mapi :: ((a, Int) -> b) -> [a] -> [b]
mapi f xs = [ f (x, i) | (x, i) <- zip xs [0..] ]

type Row = Int
type Stars = Int
type Board = [Stars]

data Player = Player1 | Player2 deriving (Show)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

initial :: Board
initial = [5, 4, 3, 2, 1]

move :: (Row, Stars) -> Board -> Board
move (row, stars) = mapi (\(s, i) -> if i == row then s - stars else s)

valid :: (Row, Stars) -> Board -> Bool
valid (row, stars) board = board !! row >= stars

gameFinished :: Board -> Bool
gameFinished = all (== 0)

getInt :: IO Int
getInt = read <$> getLine

getMove :: Player -> IO (Row, Stars)
getMove player = do putStrLn ""
                    print player
                    putStrLn "Enter a row number:"
                    row <- getInt
                    putStrLn "Enter number of stars to remove:"
                    stars <- getInt
                    putStrLn ""
                    return (row-1, stars)

printRow :: (Row, Stars) -> IO ()
printRow (row, stars) = let rowId  = show (row + 1) ++ ": "
                            stars' = intersperse ' ' (replicate stars '*')
                         in putStrLn $ rowId ++ stars'

printState :: Board -> IO ()
printState board = let rowInfo = zip [0..] board
                    in traverse_ printRow rowInfo

playGame :: Player -> Board -> IO ()
playGame currentPlayer board = do printState board
                                  m <- getMove currentPlayer
                                  if not (valid m board) then
                                      do putStrLn "Invalid move, please try again"
                                         playGame currentPlayer board
                                  else
                                      let newState = move m board
                                       in if gameFinished newState then
                                              putStrLn $ show currentPlayer ++ " wins!"
                                          else
                                              playGame (nextPlayer currentPlayer) newState

main = do print $ gameFinished . move (1, 4) $ initial
          print $ gameFinished . move (0, 5) . move (1, 4) . move (2, 3) . move (3, 2) . move (4, 1) $ initial
          playGame Player1 initial
