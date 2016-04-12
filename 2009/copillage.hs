module Main where

import Control.Monad

ask_copied :: Integer -> Integer -> [(Integer, Integer)] -> Bool
ask_copied from to = any (\(x,y) -> x == from && y == to)

test_copies :: Integer -> [(Integer, Integer)] -> IO ()
test_copies 0 _ = return ()
test_copies n arr = do
    line <- getLine
    let [a, b] = map read $ words line :: [Integer]
    if is_query a b
        then
            if ask_copied (-a) (-b) arr
                then do
                    putStrLn "1"
                    test_copies (n-1) arr
                else do
                    putStrLn "0"
                    test_copies (n-1) arr
        else do
            test_copies (n-1) ((a,b):arr)
    where
        is_query :: Integer -> Integer -> Bool
        is_query a b = a < 0 && b < 0

main :: IO ()
main = do
    str_c <- getLine
    str_n <- getLine
    let n = read str_n :: Integer
    test_copies n []
    return ()
