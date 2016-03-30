module Main where

import Control.Monad
import Data.List (transpose)

count_pattern :: [Integer] -> Int
count_pattern line = count_pattern' line + count_pattern' (reverse line)
    where
        count_pattern' :: [Integer] -> Int
        count_pattern' (_:_:[]) = 0
        count_pattern' p@(1:1:0:xs) = 1 + count_pattern' (tail p)
        count_pattern' p = count_pattern' (tail p)

get_grid :: IO [[Integer]]
get_grid = replicateM 7 $ do
    line <- getLine
    return (map (\x -> read (x:"")) line :: [Integer])

count_move :: [[Integer]] -> Int
count_move grid = sum (map count_pattern grid) + sum (map count_pattern (transpose grid))

main :: IO ()
main = do
    grid <- get_grid
    print $ count_move grid
    return ()
