module Main where

import Control.Monad

safe_tail :: [a] -> [a]
safe_tail [] = []
safe_tail (_:xs) = xs

parse_str :: String -> [Integer]
parse_str [] = []
parse_str str = nb : parse_str (safe_tail (dropWhile (/='+') str))
    where
        nb = read $ takeWhile (/='+') str

main :: IO ()
main = do
    str <- getLine
    let numbers = parse_str str
    print $ sum numbers
    return ()
