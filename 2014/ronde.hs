module Main where

import Control.Monad

main :: IO ()
main = do
    str_n <- getLine
    _ <- getLine
    let n = read str_n
    image <- replicateM n getLine
    mapM_ putStrLn $ reverse image
    return ()
