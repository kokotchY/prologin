module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    str_nb <- getLine
    let numbers = map read $ words str_nb :: [Int]
        result = map head $ filter (\l -> head l == sum (tail l)) $ permutations numbers
    print $ case result of
        [] -> 0
        (x:xs) -> x
    return ()
