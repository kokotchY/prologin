module Main where

import Control.Monad
import Data.Char
import Data.List

type Programme = [Instruction]
type Stack = [Instruction]

data Number = Decimal Int
    | Binary [Int]
    | Hexa [Char]
    | Octal [Int]
    deriving (Show, Eq)

data SOperation = Drop
    | Dup
    | Swap
    deriving (Show, Eq)

data NOperation = Add
    | Min
    | Mul
    | Div
    deriving (Show, Eq)

data BOperation = Less
    | LessEq
    | Greater
    | GreaterEq
    | Equal
    | Different
    deriving (Show, Eq)

data Instruction = Nb Number
    | StackOp SOperation
    | NumberOp NOperation
    | BoolOp BOperation
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
convertDecimal (Binary x) = Decimal $ foldl' (\x y -> x*2+y) 0 x
convertDecimal _ = error "Not implemented"

appendStack :: Stack -> Instruction -> Stack
appendStack stack op = op:stack

eval :: Programme -> Stack -> Stack
eval [] stack = stack
eval (x:xs) stack =
    case x of
        Nb number -> eval xs $ appendStack stack (Nb $ convertDecimal number)
        NumberOp op ->
            case op of
                Add -> eval xs $ execOpNumber (+) stack
                Min -> eval xs $ execOpNumber (-) stack
                Mul -> eval xs $ execOpNumber (*) stack
                Div -> eval xs $ execOpNumber (div) stack
        BoolOp op ->
            case op of
                Less -> eval xs $ execOpBool (<) stack
                LessEq -> eval xs $ execOpBool (<=) stack
                Greater -> eval xs $ execOpBool (>) stack
                GreaterEq -> eval xs $ execOpBool (>=) stack
                Equal -> eval xs $ execOpBool (==) stack
                Different -> eval xs $ execOpBool (/=) stack
        StackOp op ->
            case op of
                Drop -> eval xs $ tail stack
                Dup -> eval xs (head stack:stack)
                Swap -> eval xs $ swap stack

prog1 :: String
prog1 = "1 2 +"

exec :: Programme -> Stack
exec = flip eval []

test1, test2, test3, test4, test5, test6, test7 :: Bool
test1 = exec [Nb (Decimal 1), Nb (Decimal 2), NumberOp Add] == [Nb (Decimal 3)]
test2 = exec [Nb (Decimal 1), Nb (Decimal 2), NumberOp Min] == [Nb (Decimal 1)]
test3 = exec [Nb (Decimal 3), Nb (Decimal 2), NumberOp Mul] == [Nb (Decimal 6)]
test4 = exec [Nb (Decimal 2), Nb (Decimal 4), NumberOp Div] == [Nb (Decimal 2)]
test5 = exec [Nb (Decimal 1), Nb (Decimal 2), BoolOp Less] == [Nb (Decimal 0)]
test6 = exec [Nb (Decimal 2), Nb (Decimal 1), BoolOp Less] == [Nb (Decimal 1)]
test7 = exec [Nb (Decimal 2), Nb (Decimal 2), BoolOp LessEq] == [Nb (Decimal 1)]

testStack1, testStack2, testStack3 :: Bool
testStack1 = exec [Nb (Decimal 3), Nb (Decimal 4), StackOp Drop] == [Nb (Decimal 3)]
testStack2 = exec [Nb (Decimal 3), Nb (Decimal 4), StackOp Dup] == [Nb (Decimal 4), Nb (Decimal 4), Nb (Decimal 3)]
testStack3 = exec [Nb (Decimal 3), Nb (Decimal 4), StackOp Swap] == [Nb (Decimal 3), Nb (Decimal 4)]

allStackTests :: [Bool]
allStackTests = [testStack1, testStack2, testStack3]

allTests :: [Bool]
allTests = [test1, test2, test3, test4, test5, test6, test7]

main :: IO ()
main = do
    return ()
