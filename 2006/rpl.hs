module Main where

import Control.Monad
import Data.Char

type Programme = [Operation]
type Stack = [Number]

data Number = Decimal Int
    | Binary [Int]
    | Hexa [Char]
    | Octal [Int]
    deriving Show

data Operation = Drop
    | Dup
    | Swap
    | Add
    | Min
    | Mul
    | Div
    deriving Show

data BoolOperation = BValue Bool
    | Less
    | LessEq
    | Greater
    | GreaterEq
    | Equal
    | Different
    deriving Show



nbPlus :: Number -> Number -> Number
nbPlus (Decimal a) (Decimal b) = Decimal $ a + b
nbPlus _ _ = error "Not implemented"

nbMin :: Number -> Number -> Number
nbMin (Decimal a) (Decimal b) = Decimal $ a - b
nbMin _ _ = error "Not implemented"

nbMul :: Number -> Number -> Number
nbMul (Decimal a) (Decimal b) = Decimal $ a * b
nbMul _ _ = error "Not implemented"

nbDiv :: Number -> Number -> Number
nbDiv (Decimal a) (Decimal b) = Decimal $ a `div` b
nbDiv _ _ = error "Not implemented"

execOp :: (Number -> Number -> Number) -> Stack -> Stack
execOp f (nb1:nb2:stack) = new_val:stack
    where
        new_val = f nb1 nb2

swap :: Stack -> Stack
swap (x:y:xs) = (y:x:xs)

eval :: Programme -> Stack -> Stack
eval [] stack = stack
eval (x:xs) stack =
    case x of
        Add -> eval xs $ execOp nbPlus stack
        Min -> eval xs $ execOp nbMin stack
        Mul -> eval xs $ execOp nbMul stack
        Div -> eval xs $ execOp nbDiv stack
        Drop -> eval xs $ tail stack
        Dup -> eval xs (head stack:stack)
        Swap -> eval xs $ swap stack

prog1 = "1 2 +"


main :: IO ()
main = do
    return ()
