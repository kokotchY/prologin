module Main where

import Control.Monad

get_solution :: (Integer, Integer) -> Integer -> (Integer, Integer)
get_solution v 0 = v
get_solution (v1, v2) n = get_solution (v1*2+v2, v1) (n-1)

main :: IO ()
main = do
    str_v <- getLine
    str_n <- getLine
    let [v1, v2] = map read $ words str_v :: [Integer]
        n = read str_n :: Integer
        solution = get_solution (v1, v2) n
    print $ (fst solution + snd solution)
    return ()
