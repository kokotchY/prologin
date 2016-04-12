module Main where

import Control.Monad
import Data.List

solution :: Integer -> [Integer] -> Int
solution n list = length $ maximumBy (\x y -> compare (length x) (length y)) list'
    where
        list' = filter (\arr -> (sum arr) `mod` n == 0) (inits list ++ tails list)

main :: IO ()
main = do
    _ <- getLine
    str_numbers <- getLine
    str_n <- getLine
    let numbers = map read $ words str_numbers :: [Integer]
        n = read str_n :: Integer
    print $ solution n numbers
    return ()
