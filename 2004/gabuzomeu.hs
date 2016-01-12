module Main where

import Control.Monad

result :: Int -> [[String]]
result 0 = word_list
result n = 

word :: [String]
word = ["Bu", "Ga", "Meu", "Zo"]

word_list :: [[String]]
word_list = map (\x -> [x]) word

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n :: Int
    return ()
