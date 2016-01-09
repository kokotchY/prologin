module Main where

import Control.Monad

main :: IO ()
main = do
    _ <- getLine
    word <- getLine
    str_n <- getLine
    let n = read str_n
    dictionary <- replicateM n $ do
        _ <- getLine
        dico_word <- getLine
        return dico_word
    if word `elem` dictionary
        then print 0
        else print 1
    return ()
