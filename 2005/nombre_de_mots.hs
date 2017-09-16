module Main where

import Control.Monad

get_words :: String -> [String]
get_words = words' "" . dropWhile is_separator
    where
        words' :: String -> String -> [String]
        words' [] [] = []
        words' cur [] = [cur]
        words' cur (x:xs)
            | is_separator x = cur : words' "" (dropWhile is_separator xs)
            | otherwise = words' (cur ++ [x]) xs

is_separator :: Char -> Bool
is_separator = flip (elem) " .,"

data1 :: String
data1 = "Prologin   2005,  ca tronconne du castor..."

main :: IO ()
main = do
    line <- getLine
    print $ length $ get_words line
    return ()
