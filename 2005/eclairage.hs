module Main where

import Control.Monad

the_pattern :: [Int]
the_pattern = map (\x -> 3*x+1) [1..]

result :: [Int] -> [Int] -> [Int]
result [] lamps = lamps
result (x:xs) lamps
    | x == 1 = result xs $ map invert lamps
    | x == 2 = result xs $ reverse_even lamps
    | x == 3 = result xs $ reverse_odd lamps
    | x == 4 = result xs $ reverse_pattern lamps

invert :: Int -> Int
invert 0 = 1
invert 1 = 0

reverse_even :: [Int] -> [Int]
reverse_even lamps = map (\(p, v) -> if p then invert v else v) list_pattern
    where
        pattern = cycle [False, True]
        list_pattern = zip pattern lamps


bla l@(p:ps) ((i, x):xs)
    | xs == [] = []
    | p == i = True : bla ps xs
    | otherwise = False : bla l xs

reverse_odd :: [Int] -> [Int]
reverse_odd lamps = map (\(p, v) -> if p then invert v else v) list_pattern
    where
        pattern = cycle [True, False]
        list_pattern = zip pattern lamps

reverse_pattern :: [Int] -> [Int]
reverse_pattern lamps = map (\(p, v) -> if p then invert v else v) list_pattern
    where
        pattern = bla the_pattern (zip [1..] lamps)
        list_pattern = zip pattern lamps

main :: IO ()
main = do
    str_params <- getLine
    str_buttons <- getLine
    let params = map read $ words str_params :: [Int]
        buttons = map read $ words str_buttons :: [Int]
        n = params !! 0
        lamps = take n $ repeat 0
    print $ result buttons lamps
    return ()
