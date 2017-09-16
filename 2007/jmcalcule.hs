module Main where

import Control.Monad

eval :: [String] -> (Int, [String])
eval [] = (0, [])
eval (op:xs)
    | is_op op = (apply_op op op1 op2, remain')
    | otherwise = (read op, xs)
    where
        (op1, remain) = eval xs
        (op2, remain') = eval remain

apply_op :: String -> Int -> Int -> Int
apply_op "+" a b = a + b
apply_op "-" a b = a - b
apply_op "*" a b = a * b
apply_op "/" a b = a `div` b

is_op :: String -> Bool
is_op op = op `elem` ["+","-","*","/"]

main :: IO ()
main = do
    expr <- liftM words getLine
    let (sol, remain) = eval expr
    print sol
    return ()
