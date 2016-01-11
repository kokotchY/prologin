module Main where

import Control.Monad
import Data.Char

nb_vowel :: String -> Int
nb_vowel = length . filter (is_vowel)
    where
        is_vowel = flip elem "aeiouy"

main :: IO ()
main = do
    _ <- getLine
    str <- getLine
    let sentence = map toLower str
        result = nb_vowel sentence
    print result
    return ()
