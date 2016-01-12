module Main where

import Control.Monad
import Data.List

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = x + y
    where
        x = abs $ x1-x2
        y = abs $ y1-y2

main :: IO ()
main = do
    str_start <- getLine
    str_n <- getLine
    let n = read str_n
        [s_x, s_y] = map read $ words str_start :: [Int]
        start = (s_x, s_y)
    coords <- replicateM n $ do
        line <- getLine
        let [x, y] = map read $ words line :: [Int]
        return $ (x,y)
    print $ minimum $ map (distance start) coords
    return ()
