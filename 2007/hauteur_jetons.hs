module Main where

import Control.Monad
import Data.List

solution :: [[Int]] -> Int
solution grid = maximum $ map (length . filter (==1)) $ transpose grid

solution' :: [[Int]] -> Int
solution' = length . dropWhile (all (==0))

test_grid :: [[Int]]
test_grid = [[0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,1,0,1,0,0,1,1,0],[0,0,0,1,1,1,1,1,1,1,1,1]]

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params
        n = params !! 0
    grid <- replicateM n $ do
        line <- getLine
        let numbers = map read $ words line :: [Int]
        return $ numbers
    print $ solution' grid
    return ()
