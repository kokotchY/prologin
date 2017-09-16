module Main where

import Control.Monad
import Data.List

data1 :: [[Int]]
data1 = [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,3,4,5,6,7,8,9,1],[5,6,7,8,9,1,2,3,4],[8,9,1,2,3,4,5,6,7],[3,4,5,6,7,8,9,1,2],[6,7,8,9,1,2,3,4,5],[9,1,2,3,4,5,6,7,8]]

is_valid :: [[Int]] -> Bool
is_valid grid = all_good grid && all_good (transpose grid) && all_sub_grid_good grid

all_sub_grid_good :: [[Int]] -> Bool
all_sub_grid_good grid = all (==[1..9]) $ map (\i -> get_grid (get_coord i) grid) [1..9]

{-
1 2 3
4 5 6
7 8 9
 -}

get_grid :: ([Int],[Int]) -> [[Int]] -> [Int]
get_grid (xpos,ypos) grid = sort $ concatMap (\l -> map (\x -> l !! x) xpos) $ map (\y -> grid !! y) ypos

get_coord :: Int -> ([Int], [Int])
get_coord 1 = ([0..2], [0..2])
get_coord 2 = ([3..5], [0..2])
get_coord 3 = ([6..8], [0..2])
get_coord 4 = ([0..2], [3..5])
get_coord 5 = ([3..5], [3..5])
get_coord 6 = ([6..8], [3..5])
get_coord 7 = ([0..2], [6..8])
get_coord 8 = ([3..5], [6..8])
get_coord 9 = ([6..8], [6..8])

all_good :: [[Int]] -> Bool
all_good grid = all (\l -> sort l == [1..9]) grid

main :: IO ()
main = do
    grid <- replicateM 9 (liftM (map read . words) getLine)
    if is_valid grid
        then putStrLn "1"
        else putStrLn "0"
    return ()
