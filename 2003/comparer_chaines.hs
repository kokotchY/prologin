module Main where

import Control.Monad

my_compare :: String -> String -> Ordering
my_compare [] [] = EQ
my_compare [] (x:_) = LT
my_compare (x:_) [] = GT
my_compare (x:xs) (y:ys)
    | x == y = my_compare xs ys
    | x < y = LT
    | y < x = GT

get_first :: String -> String -> String
get_first str1 str2 =
    case my_compare str1 str2 of
        LT -> str1
        GT -> str2
        EQ -> str1

main :: IO ()
main = do
    _ <- getLine
    str1 <- getLine
    _ <- getLine
    str2 <- getLine
    putStrLn $ get_first str1 str2
    return ()
