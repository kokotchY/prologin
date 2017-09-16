module Main where

import Control.Monad

find_dup :: [Int] -> Int
find_dup = find_dup' []
    where
        find_dup' :: [Int] -> [Int] -> Int
        find_dup' seen (x:xs)
            | x `elem` seen = x
            | otherwise = find_dup' (x:seen) xs

main :: IO ()
main = do
    n <- readLn
    list <- replicateM n (liftM read getLine) :: IO [Int]
    print $ find_dup list
    return ()
