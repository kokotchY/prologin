module Main where

import Control.Monad

gen_diff :: [Int] -> [Int]
gen_diff [] = error "Empty list"
gen_diff (x:[]) = []
gen_diff (x:y:ys) = (abs $ x-y):gen_diff (y:ys)

main :: IO ()
main = do
    _ <- getLine
    str_nb <- getLine
    let numbers = map read $ words str_nb :: [Int]
        diff = gen_diff numbers
        result = maximum diff
    print result
    return ()
