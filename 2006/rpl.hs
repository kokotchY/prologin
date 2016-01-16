module Main where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map

type Programme = [Instruction]
type Stack = [Instruction]
type Variables = Map.Map VariableName Instruction
type VariableName = String

data State = State
    { getStack :: Stack
    , getVariable :: Variables
    , debugInfo :: [String]
    }
    deriving Show

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
    | AssignVariable VariableName
    | GetVariable VariableName
    | IfThenElse Programme Programme Programme
    deriving (Show, Eq)

execOpNumber :: (Int -> Int -> Int) -> State -> State
execOpNumber f state = state { getStack = Nb (Decimal (f a b)):remainStack }
    where
        stack = getStack state
        a = case (head stack) of
            Nb (Decimal x) -> x
        b = case (head $ tail stack) of
            Nb (Decimal x) -> x
        remainStack = drop 2 stack

execOpBool :: (Int -> Int -> Bool) -> State -> State
execOpBool f state
    | f a b = state { getStack = Nb (Decimal 1):remainStack }
    | otherwise = state { getStack = Nb (Decimal 0):remainStack }
    where
        stack = getStack state
        a = case (head stack) of
            Nb (Decimal x) -> x
        b = case (head $ tail stack) of
            Nb (Decimal x) -> x
        remainStack = drop 2 stack

swap :: Stack -> Stack
swap (x:y:xs) = (y:x:xs)

convertDecimal :: Number -> Number
convertDecimal (Decimal x) = Decimal x
convertDecimal (Binary x) = Decimal $ foldl' (\x y -> x*2+y) 0 x
convertDecimal _ = error "Not implemented"

appendStack :: State -> Instruction -> State
appendStack state op = state { getStack = op:(getStack state) }

addVariable :: String -> State -> State
addVariable name state = state { getVariable = variables, getStack = newStack }
    where
        variables = Map.insert name (head stack) (getVariable state)
        stack = getStack state
        newStack = tail stack

appendVariableStack :: String -> State -> State
appendVariableStack name state = state { getStack = newStack }
    where
        stack = getStack state
        variableValue = case Map.lookup name (getVariable state) of
            Just value -> value
        newStack = variableValue:stack

popStack :: State -> (Instruction, State)
popStack state = (instr, newState)
    where
        stack = getStack state
        instr = head stack
        newState = state { getStack = tail stack }

executeIfThenElse :: Programme -> Programme -> Programme -> State -> State
executeIfThenElse condProg progTrue progFalse state = newState { debugInfo = newInfo }
    where
        stateCondProg = eval condProg state
        (result, tmpState) = popStack stateCondProg
        newState = case result of
            Nb (Decimal 1) -> eval progTrue tmpState
            Nb (Decimal 0) -> eval progFalse tmpState
        newInfo = (show result) : (show newState): debugInfo newState

eval :: Programme -> State -> State
eval [] state = state
eval (x:xs) state =
    case x of
        Nb number -> eval xs $ appendStack state (Nb $ convertDecimal number)
        NumberOp op ->
            case op of
                Add -> eval xs $ execOpNumber (+) state
                Min -> eval xs $ execOpNumber (-) state
                Mul -> eval xs $ execOpNumber (*) state
                Div -> eval xs $ execOpNumber (div) state
        BoolOp op ->
            case op of
                Less -> eval xs $ execOpBool (<) state
                LessEq -> eval xs $ execOpBool (<=) state
                Greater -> eval xs $ execOpBool (>) state
                GreaterEq -> eval xs $ execOpBool (>=) state
                Equal -> eval xs $ execOpBool (==) state
                Different -> eval xs $ execOpBool (/=) state
        StackOp op ->
            case op of
                Drop -> eval xs $ state { getStack = tail $ getStack state }
                Dup -> eval xs $ state { getStack = (head (getStack state):(getStack state)) }
                Swap -> eval xs $ state { getStack = swap (getStack state) }
        AssignVariable name -> eval xs $ addVariable name state
        GetVariable name -> eval xs $ appendVariableStack name state
        IfThenElse condProg progTrue progFalse -> eval xs $ executeIfThenElse condProg progTrue progFalse state

prog1 :: String
prog1 = "1 2 +"

exec :: Programme -> State
exec = flip eval $ State [] Map.empty []

execStack :: Programme -> Stack
execStack = getStack . exec

execVariables :: Programme -> Variables
execVariables = getVariable . exec

test1, test2, test3, test4, test5, test6, test7 :: Bool
test1 = execStack [Nb (Decimal 1), Nb (Decimal 2), NumberOp Add] == [Nb (Decimal 3)]
test2 = execStack [Nb (Decimal 1), Nb (Decimal 2), NumberOp Min] == [Nb (Decimal 1)]
test3 = execStack [Nb (Decimal 3), Nb (Decimal 2), NumberOp Mul] == [Nb (Decimal 6)]
test4 = execStack [Nb (Decimal 2), Nb (Decimal 4), NumberOp Div] == [Nb (Decimal 2)]
test5 = execStack [Nb (Decimal 1), Nb (Decimal 2), BoolOp Less] == [Nb (Decimal 0)]
test6 = execStack [Nb (Decimal 2), Nb (Decimal 1), BoolOp Less] == [Nb (Decimal 1)]
test7 = execStack [Nb (Decimal 2), Nb (Decimal 2), BoolOp LessEq] == [Nb (Decimal 1)]

testStack1, testStack2, testStack3 :: Bool
testStack1 = execStack [Nb (Decimal 3), Nb (Decimal 4), StackOp Drop] == [Nb (Decimal 3)]
testStack2 = execStack [Nb (Decimal 3), Nb (Decimal 4), StackOp Dup] == [Nb (Decimal 4), Nb (Decimal 4), Nb (Decimal 3)]
testStack3 = execStack [Nb (Decimal 3), Nb (Decimal 4), StackOp Swap] == [Nb (Decimal 3), Nb (Decimal 4)]

allStackTests :: [Bool]
allStackTests = [testStack1, testStack2, testStack3]

allTests :: [Bool]
allTests = [test1, test2, test3, test4, test5, test6, test7]

main :: IO ()
main = do
    return ()
