module Main where

import Control.Monad
import Data.List

gen_pos :: [Integer] -> [[Integer]]
gen_pos [x] = []
gen_pos (x:xs) = map (\c -> x:[c]) xs ++ gen_pos xs

closest :: Integer -> [Integer] -> Integer
closest n list = maximumBy (\a b -> compare (abs (n-a)) (abs (n-b))) couples
    where
        couples = map product $ gen_pos list

main :: IO ()
main = do
    str_n <- getLine
    _ <- getLine
    str_nb <- getLine
    let n = read str_n
        list = map read $ words str_nb
    print $ closest n list
    return ()
