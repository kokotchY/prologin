module Main where

import Control.Monad

box42 :: Int -> Int -> [String]
box42 w h = [header] ++ content ++ [footer]
    where
        header = "*" ++ repeat_char (w-2) '#' ++ "+"
        content = if even w
            then concat $ take (h-2) $ repeat one_line
            else concat $ take (div (h-2) 2) $ repeat two_line
        footer = reverse header
        one_line = ["#" ++ concat (take (div (w-2) 2) (repeat "42")) ++ "#"]
        two_line = repeat "#"

repeat_char :: Int -> Char -> [Char]
repeat_char nb c = take nb $ repeat c

main :: IO ()
main = do
    str_n <- getLine
    str_m <- getLine
    let n = read str_n
        m = read str_m
        box = box42 n m
    return ()
