module Main where

import Control.Monad

power_2 :: [Int]
power_2 = map (\x -> 2^x) [0..]

is_power_2 :: Int -> [Int] -> Bool
is_power_2 nb (x:xs)
    | nb == x = True
    | nb < x = False
    | otherwise = is_power_2 nb xs

result :: Int -> Int
result m = head $ dropWhile (\x -> not (is_power_2 x cache_power_2)) [(m+1)..]
    where
        cache_power_2 = power_2

main :: IO ()
main = do
    str_m <- getLine
    let m = read str_m :: Int
    print $ result m
    return ()
