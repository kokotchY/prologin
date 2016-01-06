module Main where

import Data.List
import Data.List.Split

my_words :: String -> [String]
my_words = splitOn " "

main :: IO ()
main = do
    _ <- getLine
    _ <- getLine
    str_word_delete <- getLine
    text <- getLine
    let word = sort $ map read $ words str_word_delete
        parsed_sentence = filter (\x -> not (fst x `elem` word)) $ zip [1..] (my_words text)
    putStrLn $ unwords $ map snd parsed_sentence
    return ()
