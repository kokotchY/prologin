module Main where

import Control.Monad
import Data.Char

rot13 :: String -> String
rot13 [] = []
rot13 (x:xs) = []

{-map (chr . (+ (ord 'a')) . (flip mod 26) . (+13) . ord) ['a'..'z']-}


main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    putStrLn $ rot13 sentence
    return ()
