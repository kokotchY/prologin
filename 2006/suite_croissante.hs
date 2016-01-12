module Main where

import Control.Monad


get_growing_suite :: [Int] -> [[Int]]
get_growing_suite list = get_growing_suite' [head list] (tail list)
    where
        get_growing_suite' :: [Int] -> [Int] -> [[Int]]
        get_growing_suite' rev_acc [] = [reverse rev_acc]
        get_growing_suite' rev_acc (x:xs)
            | head rev_acc < x = get_growing_suite' (x:rev_acc) xs
            | otherwise = [reverse rev_acc] ++ get_growing_suite' [x] xs

get_max_growing_suite :: [Int] -> Int
get_max_growing_suite = maximum . map (length) . get_growing_suite

main :: IO ()
main = do
    str_numbers <- getLine
    let numbers = map read $ words str_numbers :: [Int]
    print $ case length numbers of
        0 -> 0
        nb -> get_max_growing_suite numbers
    return ()
