module Main where

import Control.Monad

min_rayon :: (Int, Int, Int, Int) -> (Int, Int) -> Int
min_rayon r@(x1, y1, x2, y2) c@(c_x, c_y)
    | circle_in_rectangle c r = minimum $ map abs [x1-c_x, x2-c_x, y1-c_y, y2-c_y]
    | otherwise = -1

circle_in_rectangle :: (Int, Int) -> (Int, Int, Int, Int) -> Bool
circle_in_rectangle (c_x, c_y) (x1, y1, x2, y2) = ((min_x <= c_x) && (c_x <= max_x)) && ((min_y <= c_y) && (c_y <= max_y))
    where
        min_x = min x1 x2
        max_x = max x1 x2
        min_y = min y1 y2
        max_y = max y1 y2

do_test x1 y1 x2 y2 c_x c_y = min_rayon (x1, y1, x2, y2) (c_x, c_y)

main :: IO ()
main = do
    str_rectangle <- getLine
    str_circle <- getLine
    let [x1, y1, x2, y2] = map read $ words str_rectangle :: [Int]
        [c1, c2] = map read $ words str_circle :: [Int]
    print $ min_rayon (x1, y1, x2, y2) (c1, c2)
    return ()
