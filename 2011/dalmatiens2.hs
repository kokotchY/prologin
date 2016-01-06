module Main where

import Control.Monad

get_parts :: Int -> [[Int]] -> ([[Int]], [[Int]])
get_parts width list = (map (take half_left) list, map (drop half_right) list)
    where
        half_left = if even width then width `div` 2 else (width `div` 2) + 1
        half_right = width `div` 2

compare_parts :: ([[Int]], [[Int]]) -> Bool
compare_parts (left, right) = left == map reverse right

get_first_half :: Int -> [[Int]] -> [[Int]]
get_first_half width list = map (take half) list
    where
        half = if even width then width `div` 2 else (width `div` 2) + 1

get_second_half :: Int -> [[Int]] -> [[Int]]
get_second_half width list = map (drop half) list
    where
        half = width `div` 2

img :: [[Int]]
img = [[0,0,1,1,0,0], [0,1,1,1,1,0], [0,0,1,1,0,0]]

img2 :: [[Int]]
img2 = [[0,0,1,1,1,0,0], [0,1,1,1,1,1,0], [0,0,1,1,1,0,0]]

main :: IO ()
main = do
    str_n <- getLine
    str_m <- getLine
    let n = read str_n :: Int
        m = read str_m :: Int
    list <- replicateM n $ do
        line <- getLine
        return $ (map read $ words line :: [Int])
    if compare_parts (get_parts m list)
        then putStrLn "1"
        else putStrLn "0"
    return ()
