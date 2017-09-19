module Main where

import Control.Monad
import Data.List

type Pos = (Int,Int)

distance :: Pos -> Pos -> Int
distance (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

calc_distance :: Pos -> [Pos] -> Int
calc_distance guichet list = first + (sum $ map ((*) 2) remain)
    where
        (first:remain) = sort $ map (distance guichet) list

main :: IO ()
main = do
    xg <- readLn
    yg <- readLn
    n <- readLn :: IO Int
    coords <- replicateM n $ do
        x <- readLn
        y <- readLn
        return (x,y) :: IO (Int,Int)
    print $ calc_distance (xg,yg) coords
    return ()
