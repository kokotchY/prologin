module Main where

import Control.Monad
import Data.Char

type Programme = [String]
type Stack = [String]

execute_op :: String -> Stack -> Stack
execute_op op (x:y:xs)
    | op == "+" = show ((read x) + (read y)) : xs
    | op == "-" = show ((read x) - (read y)) : xs

is_operator ::  String -> Bool
is_operator "+" = True
is_operator _ = False

is_digit :: String -> Bool
is_digit = all is_nb

is_nb :: Char -> Bool
is_nb nb = ord nb >= ord '0' && ord nb <= ord '9'

parse :: Programme -> Stack -> [String]
parse [] stack = stack
parse (x:xs) stack
    | is_digit x = parse xs (x:stack)
    | is_operator x = parse xs (execute_op x stack)

prog1 = "1 2 +"

main :: IO ()
main = do
    return ()
