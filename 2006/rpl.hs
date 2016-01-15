module Main where

import Control.Monad
import Data.Char

type Programme = [Instruction]
type Stack = [Instruction]

data Number = Decimal Int
    | Binary [Int]
    | Hexa [Char]
    | Octal [Int]
    deriving (Show, Eq)

data Instruction = Nb Number
    | Drop
    | Dup
    | Swap
    | Add
    | Min
    | Mul
    | Div
    | Less
    | LessEq
    | Greater
    | GreaterEq
    | Equal
    | Different
    deriving (Show, Eq)

execOpNumber :: (Int -> Int -> Int) -> Stack -> Stack
execOpNumber f (Nb (Decimal a):Nb (Decimal b):xs) = Nb (Decimal (f a b)):xs

execOpBool :: (Int -> Int -> Bool) -> Stack -> Stack
execOpBool f (Nb (Decimal a):Nb (Decimal b):xs)
    | f a b = Nb (Decimal 1):xs
    | otherwise = Nb (Decimal 0):xs

swap :: Stack -> Stack
swap (x:y:xs) = (y:x:xs)

convertDecimal :: Number -> Number
convertDecimal (Decimal x) = Decimal x
convertDecimal _ = error "Not implemented"

appendStack :: Stack -> Instruction -> Stack
appendStack stack op = op:stack

eval :: Programme -> Stack -> Stack
eval [] stack = stack
eval (x:xs) stack =
    case x of
        Nb number -> eval xs $ appendStack stack (Nb $ convertDecimal number)
        Add -> eval xs $ execOpNumber (+) stack
        Min -> eval xs $ execOpNumber (-) stack
        Mul -> eval xs $ execOpNumber (*) stack
        Div -> eval xs $ execOpNumber (div) stack
        Less -> eval xs $ execOpBool (<) stack
        LessEq -> eval xs $ execOpBool (<=) stack
        Greater -> eval xs $ execOpBool (>) stack
        GreaterEq -> eval xs $ execOpBool (>=) stack
        Equal -> eval xs $ execOpBool (==) stack
        Different -> eval xs $ execOpBool (/=) stack
        Drop -> eval xs $ tail stack
        Dup -> eval xs (head stack:stack)
        Swap -> eval xs $ swap stack

prog1 = "1 2 +"

exec :: Programme -> Stack
exec = flip eval []

main :: IO ()
main = do
    return ()
