module Main where

import Control.Monad

repeat_char :: Int -> Char -> [Char]
repeat_char nb c = take nb $ repeat c

box42' :: Int -> Int -> [String]
box42' w h = [header] ++ content ++ [footer]
    where
        header = "*" ++ repeat_char (w-2) '#' ++ "+"
        footer = reverse header
        content = gen_content (w-2) (h-2) $ cycle "42"

gen_content :: Int -> Int -> String -> [String]
gen_content w h content = gen_content' w 0 h content
    where
        gen_content' :: Int -> Int -> Int -> String -> [String]
        gen_content' w idx h content
            | idx == h = []
            | otherwise = ["#" ++ numbers ++ "#"] ++ gen_content' w (idx+1) h rest
            where
                (numbers,rest) = takeDrop w content

takeDrop :: Int -> [a] -> ([a],[a])
takeDrop nb list = (take nb list, drop nb list)

main :: IO ()
main = do
    str_n <- getLine
    str_m <- getLine
    let n = read str_n
        m = read str_m
        box = box42' n m
    mapM_ putStrLn box
    return ()
