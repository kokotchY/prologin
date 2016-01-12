module Main where

import Control.Monad
import Data.List

divisors :: Int -> [Int]
divisors n = [v | v <- [1..n], mod n v == 0]

nb_divisors :: Int -> Int
nb_divisors = length . divisors

result :: Int -> Int
result n = snd $ foldr (\x max@(nb_div, nb) -> if nb_divisors x >= nb_div then (nb_divisors x,x) else max) (0,0) [1..n]

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    print $ result n
    return ()
