module Main where

import Control.Monad


max_length :: [Integer] -> Integer
max_length list = max_length' (tail list) (head list) 1 1
    where
        max_length' :: [Integer] -> Integer -> Integer -> Integer -> Integer
        max_length' [] _ cur_max m = max cur_max m
        max_length' (x:xs) cur cur_max m
            | x == cur + 1 = max_length' xs x (cur_max+1) m
            | otherwise = max_length' xs x 1 (max cur_max m)

main :: IO ()
main = do
    _ <- getLine
    str_numbers <- getLine
    let numbers = map read $ words str_numbers
    print $ max_length numbers
    return ()
