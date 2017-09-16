module Main where

import Control.Monad
import Data.List

convert :: Int -> [Int]
convert 1 = cycle [-1]
convert 2 = cycle [1, -1]
convert 3 = cycle [-1, 1]
convert 4 = [if mod (i-1) 3 == 0 then (-1) else 1 | i <- [1..]]

compute_pattern :: [[Int]] -> [Int]
compute_pattern = undefined

convert_response :: Int -> Char
convert_response 1 = '0'
convert_response (-1) = '1'

main :: IO ()
main = do
    [b,n] <- liftM (map read . words) getLine :: IO [Int]
    button <- liftM (map (convert . read) . words) getLine :: IO [[Int]]
    let pattern = map product $ transpose $ map (take b) button
    putStrLn $ map convert_response pattern
    return ()
