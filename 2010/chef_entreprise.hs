module Main where

import Control.Monad

main :: IO ()
main = do
    str_b <- getLine
    let b = map read $ words str_b :: [Int]
    putStrLn $ show (maximum b) ++ " " ++ show (minimum b)
    return ()
