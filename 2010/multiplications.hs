module Main where

import Control.Monad
import Data.List (sort)

calc :: [Int] -> [Int]
calc [] = []
calc (x:xs) = map (*x) xs ++ calc xs

result :: [Int] -> Int
result = maximum . calc

result2 :: [Int] -> Int
result2 list = max a b
    where
        sorted = sort list
        a = product $ take 2 sorted
        b = product $ take 2 $ reverse sorted

main :: IO ()
main = do
    _ <- getLine
    str_nb <- getLine
    let numbers = map read $ words str_nb :: [Int]
    print $ result2 numbers
    return ()
