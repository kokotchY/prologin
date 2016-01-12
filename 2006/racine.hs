module Main where

import Control.Monad

my_sqrt :: Int -> Int
my_sqrt n = head $ dropWhile (\x -> (x^2 < n && (x+1)^2 <= n)) [1..(n-1)]

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    print $ my_sqrt n
    return ()
