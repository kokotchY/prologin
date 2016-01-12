module Main where

import Control.Monad
import Data.List

nb_words :: String -> Int
nb_words = length . filter and . group . map (not . is_separator)

is_separator :: Char -> Bool
is_separator = flip elem " ,."

main :: IO ()
main = do
    sentence <- getLine
    print $ nb_words sentence
    return ()
