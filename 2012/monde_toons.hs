module Main where

import Control.Monad
import Data.List

get_max :: [Int] -> Int
get_max list = max i t
    where
        i = maximum $ map sum $ inits list
        t = maximum $ map sum $ tails list

get_list :: [Int] -> [[Int]]
get_list [] = [[]]
get_list (x:xs) = map (x:) xs ++ xs

main :: IO ()
main = do
    _ <- getLine
    str_values <- getLine
    let values = map read $ words str_values :: [Int]
    print $ get_max values
    return ()
