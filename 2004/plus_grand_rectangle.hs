module Main where

import Control.Monad

type Grid = [[Integer]]

type Pos = (Int, Int)

all_zero :: Grid -> Bool
all_zero = all (==0) . concat

all_rectangles :: Pos -> Grid -> [Grid]
all_rectangles pos grid = filter all_zero $ map (\(a,b) -> get_rectangle pos a b grid) positions
    where
        positions = [(x,y) | x <- [0..w], y <- [0..h]]
        w = max_width pos grid
        h = max_height pos grid

get_rectangle :: Pos -> Int -> Int -> Grid -> Grid
get_rectangle (x,y) w h grid = grid'
    where
        grid' = map (take w . drop x) $ take h $ drop y grid

test_grid :: Grid
test_grid = [[0,1,0,1],[1,0,0,0],[0,0,0,0],[0,0,0,0],[1,1,0,1]]

max_width :: Pos -> Grid -> Int
max_width (x,y) grid = length $ takeWhile (==0) line
    where
        line = drop x $ head $ drop y grid

max_height :: Pos -> Grid -> Int
max_height (x,y) grid = length $ takeWhile (==0) row
    where
        row = drop y $ map (head . drop x) grid

main :: IO ()
main = do
     return ()
