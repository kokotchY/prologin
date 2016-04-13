module Main where

import Control.Monad

data Pattern = Simple Char | Multiple Char
    deriving Show

type Regex = [Pattern]

parse_menu :: String -> Regex
parse_menu [] = []
parse_menu [x] = [Simple x]
parse_menu (x:xs@(y:ys))
    | y == '*' = Multiple x : parse_menu ys
    | otherwise = Simple x : parse_menu xs

read_menu :: String -> Regex -> String
read_menu [] _ = []
read_menu _ [] = []
read_menu (x:xs) (p:ps) =
    case p of
        Simple c ->
            if x == c
                then x:read_menu xs ps
                else []
        Multiple c ->
            if x == c
                then x:biggest_read xs (p:ps)
                else biggest_read (x:xs) ps

biggest_read :: String -> Regex -> String
biggest_read s [] = []
biggest_read s (p:ps)
    | length (keep_pattern) >= length (pass_pattern) = keep_pattern
    | length (pass_pattern) > length (keep_pattern) = pass_pattern
    where
        keep_pattern = read_menu s (p:ps)
        pass_pattern = read_menu s ps

test_menu :: String -> String -> String
test_menu (x:xs) [] = []
test_menu [] (c:cs) = []
test_menu (x:[]) (c:cs) = if x == c then [c] else []
test_menu m@(x:y:xs) (c:cs)
    | y == '*' && x == c = c:test_menu m cs
    | y == '*' && not (x == c) = test_menu xs (c:cs)
    | not (y == '*') && x == c = c:test_menu (y:xs) cs
    | otherwise = []

main :: IO ()
main = do
    _ <- getLine
    _ <- getLine
    menu <- getLine
    command <- getLine
    print $ length $ read_menu command $ parse_menu menu
    return ()
