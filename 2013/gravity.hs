module Main where

import Control.Monad

gravity :: [[String]] -> [[String]]
gravity list = 

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params :: [Int]
        l = params !! 0
        c = params !! 1
    grid <- replicateM l getLine

