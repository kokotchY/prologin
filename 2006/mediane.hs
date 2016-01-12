module Main where

import Control.Monad
import Data.List

my_partition :: Int -> [Int] -> ([Int], [Int])
my_partition _ [] = ([], [])
my_partition nb (x:xs)
    | x < nb = (x:l, h)
    | x > nb = (l, x:h)
    | otherwise = (l, h)
    where
        (l, h) = my_partition nb xs

mediane_for :: Int -> [Int] -> Int
mediane_for nb list = abs $ length h - length l
    where
        (l, h) = my_partition nb list

mediane :: [Int] -> Int
mediane list = minimumBy (\x y -> compare (mediane_for x list) (mediane_for y list)) list

main :: IO ()
main = do
    str_nb <- getLine
    let numbers = map read $ words str_nb
    print $ mediane numbers
    return ()
