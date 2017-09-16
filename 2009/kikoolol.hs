module Main where

import Control.Monad
import Data.Char

is_invalid :: String -> Int -> Bool
is_invalid str n = count < nb_word*2
    where
        sentence = words str
        nb_word = length sentence
        bad_words = filter ((<n) . length) $ filter (not . null) $ map strip sentence
        count = length bad_words

strip :: String -> String
strip = filter isAlpha

main :: IO ()
main = do
    line <- getLine
    n <- readLn
    if is_invalid line n
        then putStrLn "0"
        else putStrLn "1"
    return ()
