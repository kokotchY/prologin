module Main where

import Control.Monad
import Data.List

compare_arr :: [Int] -> [Int] -> Ordering
compare_arr a1 a2
    | res_length == EQ = compare (last a2) (last a1)
    | otherwise = res_length
    where
        res_length = compare (length a1) (length a2)

divis :: Int -> Int
divis n = last $ maximumBy compare_arr $ map list_divis [1..n]

list_divis :: Int -> [Int]
list_divis n = filter ((==) 0 . mod n) [1..n]

main :: IO ()
main = do
    n <- readLn
    print $ divis n
    return ()
