module Main where

import Control.Monad

get_pos :: Integer -> [Integer] -> Integer
get_pos d years = fst $ head $ dropWhile (\x -> snd x < -42000) $ zip [1..] years

main :: IO ()
main = do
    str_d <- getLine
    _ <- getLine
    str_lines <- getLine
    let years = map read $ words str_lines :: [Integer]
        d = read str_d :: Integer
    print $ get_pos d years
    return ()
