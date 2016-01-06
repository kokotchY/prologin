module Main where

main :: IO ()
main = do
    _ <- getLine
    str_qi <- getLine
    let line = map read $ words str_qi :: [Int]
        nb = minimum line `div` length line
    print nb
