module Main where

import Control.Monad
import Data.Char

type Programme = [Instruction]
type Stack = [Instruction]

data Number = Decimal Int
    | Binary [Int]
    | Hexa [Char]
    | Octal [Int]
    deriving Show

data Instruction = Nb Number
    | Drop
    | Dup
    | Swap
    | Add
    | Min
    | Mul
    | Div
    deriving Show

data BoolOperation = Less
    | LessEq
    | Greater
    | GreaterEq
    | Equal
    | Different
    deriving Show



nbPlus :: Instruction -> Instruction -> Instruction
nbPlus (Nb (Decimal a)) (Nb (Decimal b)) = Nb $ Decimal $ a + b
nbPlus _ _ = error "Not implemented"

nbMin :: Instruction -> Instruction -> Instruction
nbMin (Nb (Decimal a)) (Nb (Decimal b)) = Nb $ Decimal $ a - b
nbMin _ _ = error "Not implemented"

nbMul :: Instruction -> Instruction -> Instruction
nbMul (Nb (Decimal a)) (Nb (Decimal b)) = Nb $ Decimal $ a * b
nbMul _ _ = error "Not implemented"

nbDiv :: Instruction -> Instruction -> Instruction
nbDiv (Nb (Decimal a)) (Nb (Decimal b)) = Nb $ Decimal $ a `div` b
nbDiv _ _ = error "Not implemented"

execOp :: (Instruction -> Instruction -> Instruction) -> Stack -> Stack
execOp f (i1:i2:stack) = new_val:stack
    where
        new_val = f i1 i2

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
        Add -> eval xs $ execOp nbPlus stack
        Min -> eval xs $ execOp nbMin stack
        Mul -> eval xs $ execOp nbMul stack
        Div -> eval xs $ execOp nbDiv stack
        Drop -> eval xs $ tail stack
        Dup -> eval xs (head stack:stack)
        Swap -> eval xs $ swap stack

prog1 = "1 2 +"

exec :: Programme -> Stack
exec = flip eval []

main :: IO ()
main = do
    return ()
