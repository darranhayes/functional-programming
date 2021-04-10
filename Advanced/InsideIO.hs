-- https://wiki.haskell.org/IO_inside

-- define a new language construct here, while, that performs side-effects
while :: IO Bool -> IO ()
while action = do b <- action
                  if b then do while action else return ()

multiAct :: IO Bool
multiAct = do s <- getLine
              if length s > 5 then multiAct else return False

singleAct :: IO Bool
singleAct = do s <- getLine
               print $ length s
               return False
main = do while singleAct
          while multiAct -- repeat reading line, until user entered <= 5 chars, and then terminate