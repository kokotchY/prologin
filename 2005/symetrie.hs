module Main where

import Control.Monad
import Data.List

data1, data2, data3, data4 :: [[Integer]]
data1 = [[0,0,1,1,0,0],[0,1,1,1,1,0],[0,0,1,1,0,0]]
data2 = [[0,0,1,1,1,0,0],[0,1,1,1,1,1,0],[0,0,1,1,1,0,0]]
data3 = [[0,0,0,0,0]]
data4 = [[0,0,0,1,1,0,0],[0,0,1,1,1,1,0],[0,0,0,1,1,0,0]]
data5 = [[0,0,0,1,1,0,0],[1,0,1,1,1,1,0],[0,0,0,1,1,0,0]]

all_empty :: [Integer] -> Bool
all_empty = all (==0)

remove_empty :: [[Integer]] -> [[Integer]]
remove_empty = dropWhile all_empty

prepare_grid :: [[Integer]] -> [[Integer]]
prepare_grid = remove_empty . reverse . remove_empty . transpose

has_symetry :: [[Integer]] -> Bool
has_symetry [] = True
has_symetry list
    | length list == 1 = True
    | head list == last list = has_symetry (tail $ init list)
    | otherwise = False


main :: IO ()
main = do
    [l,c] <- liftM (map read . words) getLine :: IO [Int]
    grid <- replicateM l (liftM (map read . words) getLine) :: IO [[Integer]]
    putStrLn $ if has_symetry $ prepare_grid grid
        then "1"
        else "0"
    return ()
