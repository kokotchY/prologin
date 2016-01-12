module Main where

import Control.Monad

bien_parenthesee :: String -> Bool
bien_parenthesee sentence =
    case parse_sentence sentence 0 of
        Right v -> v
        Left error -> False

parse_sentence :: String -> Int -> Either Int Bool
parse_sentence [] 0 = Right True
parse_sentence [] nb = Left nb
parse_sentence (x:xs) nb
    | x == '(' = parse_sentence xs (nb+1)
    | x == ')' && nb > 0 = parse_sentence xs (nb-1)
    | x == ')' && nb <= 0 = Left nb
    | otherwise = parse_sentence xs nb

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    if bien_parenthesee sentence
        then print 1
        else print 0
    return ()
