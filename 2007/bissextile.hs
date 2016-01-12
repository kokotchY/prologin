module Main where

import Control.Monad

is_bissextile :: Int -> Bool
is_bissextile nb
    | not (nb `mod` 4 == 0) = False
    | not (nb `mod` 100 == 0) = True
    | not (nb `mod` 400 == 0) = False
    | otherwise = True

main :: IO ()
main = do
    str_y <- getLine
    let year = read str_y :: Int
    return ()
