module Main where

import Control.Monad

losange :: Int -> Char -> [String]
losange n c = map (\(w, n_c) -> gen_line w n_c c) lines
    where
        lines = parts ++ [(0, n)] ++ (reverse parts)
        parts = map (\x -> (div n 2 - x, 2*x+1)) [0..(div n 2)-1]

gen_line :: Int -> Int -> Char -> String
gen_line nb_white nb_char c = white ++ char
    where
        white = gen_char nb_white ' '
        char = gen_char nb_char c

gen_char :: Int -> Char -> String
gen_char n c = take n $ repeat c

main :: IO ()
main = do
    str_n <- getLine
    str_c <- getLine
    let n = read str_n
        c = str_c !! 0
    mapM_ putStrLn $ losange n c
    return ()
