module Main where

import Control.Monad

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

main :: IO ()
main = do
    str_size <- getLine
    str_j_pos <- getLine
    str_k <- getLine
    let size = map read $ words str_size :: [Int]
        h = size !! 0
        w = size !! 1
        [pos_x, pos_y] = map read $ words str_j_pos :: [Int]
        k = read str_k
    list <- replicateM k $ do
        line <- getLine
        let pos = map read $ words line
            x = pos !! 0 :: Int
            y = pos !! 1 :: Int
        return (x,y)
    let j = (pos_x, pos_y)
    let closest_dist = foldr (\f min -> if distance j f < min then distance j f else min) 9999 list
    print closest_dist
    return ()
