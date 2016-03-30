module Main where

import Control.Monad
import Data.Char

rot13 :: String -> String
rot13 [] = []
rot13 (x:xs)
    | isAlpha x = c:rot13 xs
    | otherwise = x:rot13 xs
    where
        base = if isUpper x
            then ord 'A'
            else ord 'a'
        c = chr $ ((ord x - base + 13) `mod` 26) + base

{-map (chr . (+ (ord 'a')) . (flip mod 26) . (+13) . ord) ['a'..'z']-}


main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    putStrLn $ rot13 sentence
    return ()
