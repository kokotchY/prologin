module Main where

main :: IO ()
main = do
    _ <- getLine
    str_length <- getLine
    str_speeds <- getLine
    str_positions <- getLine
    let length = read str_length :: Integer
        speeds = map read $ words str_speeds :: [Integer]
        positions = map read $ words str_positions :: [Integer]
    return ()
