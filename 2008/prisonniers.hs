module Main where

import Control.Monad

type Door = (Int,Bool)

every_n :: Int -> [Int]
every_n n = map (\x -> (x+1)*n) [0..]

apply_n :: [Door] -> Int -> [Door]
apply_n doors n = apply_n' (every_n n) doors
    where
        apply_n' :: [Int] -> [Door] -> [Door]
        apply_n' _ [] = []
        apply_n' (x:xs) (d@(d_i, d_v):ds)
            | x == d_i = (d_i, not d_v):apply_n' xs ds
            | otherwise = d:apply_n' (x:xs) ds

main :: IO ()
main = do
    str_n <- getLine
    str_p <- getLine
    let n = read str_n :: Int
        p = read str_p :: Int
    return ()
