module InteractiveProgramming where

import System.IO (hSetEcho, stdin)

-- hangman :: IO ()
-- player a secretly types in a word
-- player b tries to deduce word by entering a sequence of guesses
-- for each guess, indicate which letters in the secret word occur in the guess
-- game ends when guess is correct

getCh :: IO Char
getCh = do hSetEcho stdin False -- do not echo values from stdin channel
           x <- getChar
           hSetEcho stdin True -- echo values from stdin channel
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                  return []
              else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)

match :: String -> String -> String
match w g = [ if elem x g then x else '-' | x <- w ]

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                   putStrLn "You won"
               else
                   do putStrLn (match word guess)
                      play word

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putChar '\n'
             putStrLn "Try to guess it:"
             play word

main = do
    hangman