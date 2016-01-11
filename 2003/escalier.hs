module Main where

import Control.Monad

main :: IO ()
main = do
    str_m <- getLine
    let m = read str_m
    mapM_ (\x -> putStrLn $ concat $ take x $ repeat "X") [1..m]
    return ()
