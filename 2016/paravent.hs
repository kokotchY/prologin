module Main where

import Control.Monad
import Data.List

solution :: [Integer] -> Int
solution [] = 0
solution (x:xs) = length (fst parts) + solution (snd parts)
    where
        parts = span (<x) xs

main :: IO ()
main = do
    _ <- getLine
    str_h <- getLine
    let heights = map read $ words str_h :: [Integer]
    print $ solution heights
    return ()
