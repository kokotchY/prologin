module Main where

import Control.Monad

main :: IO ()
main = do
    str_n <- getLine
    _ <- getLine
    let n = read str_n
    str_bags <- getLine
    let bags = map read $ words str_bags :: [Int]
        possible_bags = filter ((==0) . flip mod n) bags
    if length (possible_bags) == 0
        then print 0
        else print $ maximum possible_bags
    return ()
