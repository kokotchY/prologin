module Main where

import Control.Monad

triangle :: Int -> [String]
triangle 1 = ["*"]
triangle n = triangle (n-1) ++ [concat $ take n $ repeat "*"]

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
        t = triangle n
    mapM_ putStrLn t
    return ()
