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

prog1 :: String
prog1 = "1 2 +"

exec :: Programme -> Stack
exec = flip eval []

test1, test2, test3, test4, test5, test6, test7 :: Bool
test1 = exec [Nb (Decimal 1), Nb (Decimal 2), Add] == [Nb (Decimal 3)]
test2 = exec [Nb (Decimal 1), Nb (Decimal 2), Min] == [Nb (Decimal 1)]
test3 = exec [Nb (Decimal 3), Nb (Decimal 2), Mul] == [Nb (Decimal 6)]
test4 = exec [Nb (Decimal 2), Nb (Decimal 4), Div] == [Nb (Decimal 2)]
test5 = exec [Nb (Decimal 1), Nb (Decimal 2), Less] == [Nb (Decimal 0)]
test6 = exec [Nb (Decimal 2), Nb (Decimal 1), Less] == [Nb (Decimal 1)]
test7 = exec [Nb (Decimal 2), Nb (Decimal 2), LessEq] == [Nb (Decimal 1)]

testStack1, testStack2, testStack3 :: Bool
testStack1 = exec [Nb (Decimal 3), Nb (Decimal 4), Drop] == [Nb (Decimal 3)]
testStack2 = exec [Nb (Decimal 3), Nb (Decimal 4), Dup] == [Nb (Decimal 4), Nb (Decimal 4), Nb (Decimal 3)]
testStack3 = exec [Nb (Decimal 3), Nb (Decimal 4), Swap] == [Nb (Decimal 3), Nb (Decimal 4)]

allStackTests :: [Bool]
allStackTests = [testStack1, testStack2, testStack3]

allTests :: [Bool]
allTests = [test1, test2, test3, test4, test5, test6, test7]

main :: IO ()
main = do
    return ()
