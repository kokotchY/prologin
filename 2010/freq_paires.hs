module Main where

import Control.Monad
import Data.Char

generate_pair :: String -> [String]
generate_pair [] = []
generate_pair (x:[]) = []
generate_pair (x:y:ys)
    | is_valid x && is_valid y = [x:y:[]] ++ generate_pair (y:ys)
    | otherwise = generate_pair (y:ys)
    where
        is_valid :: Char -> Bool
        is_valid = not . flip elem [' ', '.']


generate_uniq_pair :: String -> [String]
generate_uniq_pair = unique . generate_pair

occ :: Eq a => a -> [a] -> Int
occ a = length . filter (==a)

unique :: Eq a => [a] -> [a]
unique = foldr (\x xs -> if not (x `elem` xs) then (x:xs) else xs) []

main :: IO ()
main = do
    _ <- getLine
    str_text <- getLine
    let text = map toLower str_text
        pair = generate_pair text
        uniq = unique pair
        result = maximum $ map (flip occ (generate_pair text)) uniq
    print result
    return ()
