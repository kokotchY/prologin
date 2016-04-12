module Main where

import Control.Monad

is_bissextile :: Int -> Bool
is_bissextile nb
    | nb `mod` 4 == 0 && not (nb `mod` 100 == 0) = True
    | nb `mod` 400 == 0 = True
    | otherwise = False

main :: IO ()
main = do
    str_y <- getLine
    let year = read str_y :: Int
    if is_bissextile year
        then putStrLn "1"
        else putStrLn "0"
    return ()
