module Main where

import Control.Monad
import Data.List

is_kikoolol :: String -> Integer -> Bool
is_kikoolol sentence l = nb_bad_w > (nb_words `div` 2)
    where
        w = my_words sentence
        nb_words = length w
        bad_w = filter ((<= 3) . length) w
        nb_bad_w = length bad_w

my_words :: String -> [String]
my_words [] = []
my_words sentence = w'
    where
        (w, tail) = span (not . is_separator) $ dropWhile is_separator sentence
        w' = if w == []
            then my_words tail
            else w:my_words tail

is_separator :: Char -> Bool
is_separator c = not (c `elem` ['a'..'z'] ++ ['A'..'Z'])

main :: IO ()
main = do
    sentence <- getLine
    str_n <- getLine
    let n = read str_n
    print $ if is_kikoolol sentence n
        then 1
        else 0
    return ()
