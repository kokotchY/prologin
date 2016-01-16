module Main where

import Control.Monad
import Data.List

gravity :: [[Char]] -> [[Char]]
gravity = transpose . map ((\(x,y) -> y ++ x) . partition (/='.')) . transpose

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params :: [Int]
        l = params !! 0
        c = params !! 1
    grid <- replicateM l getLine
    mapM_ putStrLn $ gravity grid
