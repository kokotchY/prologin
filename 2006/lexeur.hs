module Main where

import Control.Monad
import Data.List

data Element = SPACE | DOT | STRING deriving (Show, Eq)

parse :: String -> [Element]
parse [] = []
parse (x:xs)
    | x == ' ' = SPACE : parse xs
    | x == '.' = DOT : parse xs
    | otherwise = STRING : parse (dropWhile (not . flip elem " .") xs)

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    putStrLn $ concat $ intersperse " " $ map show $ parse sentence
    return ()
