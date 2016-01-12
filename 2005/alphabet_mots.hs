module Main where

import Control.Monad

valid_entry :: String -> String -> Bool
valid_entry alphabet word = all (flip elem alphabet) word

valid_words :: String -> [String] -> [String]
valid_words alphabet = filter (valid_entry alphabet)

main :: IO ()
main = do
    alphabet <- getLine
    str_n <- getLine
    let n = read str_n
    dictionary <- replicateM n getLine
    print $ length $ valid_words alphabet dictionary
    return ()
