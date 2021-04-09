module InteractiveProgramming where

import Prelude hiding (getLine, putStr, putStrLn)

-- Haskell is pure, no side effects
-- Interaction, files, networks, have side-effects
-- Haskell uses types to distinguish side-effectful functions
-- IO a -- the type of actions that return a value of type a
-- IO Char - actions that return a character
-- IO () - only has side effects, no result value
-- () - is the empty tuple

-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- return :: a -> IO a - returns a value without any interaction
-- once in the world of impure operations, we cannot get back to purity
-- we can go from pure to impure, but not from impure to pure

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         getChar
         return (x,y)

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                 return [] -- or return ""
             else
                 do xs <- getLine
                    return (x:xs) -- last action in any do block, must 'return' something. That's why it's called 'return'

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs -- this expression returns IO () which matches the function's type, no explicit return needed here

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

main = do
    xs <- getLine
    putStrLn xs
    strlen