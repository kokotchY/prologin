module Main where

import Control.Monad

xor :: Int -> Int -> Int
xor 0 0 = 0
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0


xor_nb :: [Int] -> [Int] -> [Int]
xor_nb = zipWith xor

main :: IO ()
main = do
    _ <- getLine
    str_nb1 <- getLine
    str_nb2 <- getLine
    let nb1 = map (read . (:"")) str_nb1 :: [Int]
        nb2 = map (read . (:"")) str_nb2 :: [Int]
        result = xor_nb nb1 nb2
    putStrLn $ concatMap show result
    return ()
