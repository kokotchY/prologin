module Main where

import Control.Monad

group_ascending :: [Integer] -> [[Integer]]
group_ascending (x:xs) = group_ascending' x [x] xs
    where
        group_ascending' :: Integer -> [Integer] -> [Integer] -> [[Integer]]
        group_ascending' last acc [] = [acc]
        group_ascending' last acc (x:xs)
            | x >= last = group_ascending' x (x:acc) xs
            | otherwise = acc : group_ascending' x [x] xs

get_profit :: [Int] -> Int
get_profit [] = 0
get_profit (x:xs) = max (get_profit_for x xs) $ get_profit xs

safe_maximum :: [Int] -> Int
safe_maximum [] = 0
safe_maximum list = maximum list

get_profit_for :: Int -> [Int] -> Int
get_profit_for p = safe_maximum . map (flip (-)p)

main :: IO ()
main = do
    _ <- getLine
    str_prices <- getLine
    let prices = map read $ words str_prices :: [Int]
    print $ get_profit prices
    return ()
