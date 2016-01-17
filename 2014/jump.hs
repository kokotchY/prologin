module Main where

import Control.Monad


bla :: [Int] -> [Bool]
bla [] = [False]
bla (x:[]) = [False]
bla (x:y:[]) = [y*2 <= x]
bla (x:y:z:xs) = [reach_left, reach_right] ++ bla (y:z:xs)
    where
        reach_left = y <= (x `div` 2)
        reach_right = y*2 <= y

main :: IO ()
main = do
    _ <- getLine
    str_size <- getLine
    let size = map read $ words str_size :: [Int]
    return ()

bla [2,3,2,xs] = []
