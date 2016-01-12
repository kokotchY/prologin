module Main where

import Control.Monad

nb_not_null :: [Int] -> Int
nb_not_null = length . filter (/= 0)

main :: IO ()
main = do
    str_nb <- getLine
    let numbers = map read $ words str_nb
        result = nb_not_null numbers
    print result
    return ()
