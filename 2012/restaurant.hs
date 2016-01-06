module Main where

import Control.Monad

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
    print $ length $ test_menu menu command
    return ()
