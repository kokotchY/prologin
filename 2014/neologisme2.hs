module Main where

import Control.Monad

data Result = Result
    { getResult :: Int
    , getRemain :: Int
    }

test_dico :: String -> Int -> IO Result
test_dico word 0 = return $ Result 1 0
test_dico word n = do
    _ <- getLine
    line <- getLine
    if line == word
        then return $ Result 0 n
        else test_dico word (n-1)

main :: IO ()
main = do
    _ <- getLine
    word <- getLine
    str_n <- getLine
    let n = read str_n
    res <- test_dico word n
    replicateM_ ((getRemain res - 1) * 2) $ do
        _ <- getLine
        _ <- getLine
        putStrLn "Read input"
    print $ getResult res
    return ()
