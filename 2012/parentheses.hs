module Main where

import Control.Monad

get_nth_word :: Int -> String -> String
get_nth_word = get_nth_word' 0 0 0
    where
        get_nth_word' :: Int -> Int -> Int -> Int -> String -> String
        get_nth_word' p_level w_p_level w_nb w (x:xs)
            | x == '(' && w_nb < w = get_nth_word' (p_level+1) w_p_level (w_nb+1) w xs
            | x == ')' && w_nb < w = get_nth_word' (p_level-1) w_p_level w_nb w xs
            | x == '(' && w_nb == w = get_nth_word' p_level p_level w_nb w xs
            | w_nb == w && x /= ')' = x:get_nth_word' p_level w_p_level w_nb w xs
            | w_nb == w && x == '(' = get_nth_word' (p_level+1) w_p_level w_nb w xs
            | w_nb == w && x == ')' && w_p_level /= p_level = get_nth_word' (p_level-1) w_p_level w_nb w xs
            | w_nb == w && x == ')' && w_p_level == p_level = []

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    str_p <- getLine
    let p = read str_p :: Int
    print $ get_nth_word p sentence
    return ()
