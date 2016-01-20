module RPL  ( execResult, exec, execVariables, execStack, parse) where

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
    deriving (Eq)

instance Show Number where
    show (Decimal d) = show d
    show (Binary b) = (concatMap show b) ++ "b"
    show (Hexa h) = (foldr (:) [] h) ++ "h"
    show (Octal o) = (concatMap show o) ++ "o"

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
    | For VariableName Programme
    | Unknown String
    deriving (Eq)

instance Show Instruction where
    show (Nb number) = show number
    show _ = "Should not be shown"

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

addVariableWithValue :: VariableName -> Instruction -> State -> State
addVariableWithValue name value state = state { getVariable = variables }
    where
        variables = Map.insert name value (getVariable state)

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
executeIfThenElse condProg progTrue progFalse state = newState
    where
        stateCondProg = eval condProg state
        (result, tmpState) = popStack stateCondProg
        newState = case result of
            Nb (Decimal 1) -> eval progTrue tmpState
            Nb (Decimal 0) -> eval progFalse tmpState

range :: Instruction -> Instruction -> [Instruction]
range start end = map (Nb . Decimal) [s..e]
    where
        s = case start of
            Nb (Decimal x) -> x
        e = case end of
            Nb (Decimal x) -> x

executeFor :: VariableName -> Programme -> State -> State
executeFor iterVariable loopProg state = newState
    where
        (endRange, state1) = popStack state
        (startRange, state2) = popStack state1
        numbers = range startRange endRange
        newState = foldl' (\s nb -> eval loopProg (addVariableWithValue iterVariable nb s)) state2 numbers

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
        For name loopProg -> eval xs $ executeFor name loopProg state

prog1 :: String
prog1 = "1 2 +"

exec :: Programme -> State
exec = flip eval $ State [] Map.empty []

execResult :: Programme -> String
execResult prog = show $ head stack
    where
        stack = getStack $ eval prog (State [] Map.empty [])

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


prog2 :: String
prog2 = "0 -> c 1 10 for i c 1 + -> c next c"

prog2Prog :: Programme
prog2Prog = [Nb (Decimal 0), AssignVariable "c", Nb (Decimal 1), Nb (Decimal 10), For "i" [GetVariable "c" , Nb (Decimal 1) , NumberOp Add , AssignVariable "c" ], GetVariable "c" ]

parse :: String -> Programme
parse prog = fst $ parse' (words prog)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

parseDecimal :: String -> Maybe Number
parseDecimal nb = case readMaybe nb of
    Just value -> Just $ Decimal value
    _ -> Nothing

parseBinary :: String -> Maybe Number
parseBinary str
    | and (map (`elem` "01") str) = Just $ Binary $ map (read . (:"")) str
    | otherwise = Nothing

parseOctal :: String -> Maybe Number
parseOctal str
    | and (map (`elem` ['0'..'7']) str) = Just $ Octal $ map (read . (:"")) str
    | otherwise = Nothing

parseHexa :: String -> Maybe Number
parseHexa str
    | and (map (`elem` ['0'..'9'] ++ ['a'..'e'] ++ ['A'..'E']) str) = Just $ Hexa str
    | otherwise = Nothing

parseNumber :: String -> Maybe Number
parseNumber str = case (last str) of
    'b' -> parseBinary number
    'B' -> parseBinary number
    'o' -> parseOctal number
    'O' -> parseOctal number
    'h' -> parseHexa number
    'H' -> parseHexa number
    _ -> parseDecimal str
    where
        number = init str

tryParseNumber :: String -> Maybe Number
tryParseNumber str
    | isDigit (head str) = case parseNumber str of
            Just nb -> Just nb
            Nothing -> Nothing
    | last str `elem` "bBoOhH" = case parseNumber str of
            Just nb -> Just nb
            Nothing -> Nothing
    | otherwise = Nothing

tryParseNumberOperation :: String -> Maybe NOperation
tryParseNumberOperation (x:[])
    | x == '+' = Just Add
    | x == '-' = Just Min
    | x == '*' = Just Mul
    | x == '/' = Just Div
    | otherwise = Nothing
tryParseNumberOperation _ = Nothing

tryParseBoolOperation :: String -> Maybe BOperation
tryParseBoolOperation "<" = Just Less
tryParseBoolOperation "<=" = Just LessEq
tryParseBoolOperation ">" = Just Greater
tryParseBoolOperation ">=" = Just GreaterEq
tryParseBoolOperation "=" = Just Equal
tryParseBoolOperation "<>" = Just Different
tryParseBoolOperation _ = Nothing

tryParseStackOperation :: String -> Maybe SOperation
tryParseStackOperation "DROP" = Just Drop
tryParseStackOperation "DUP" = Just Dup
tryParseStackOperation "SWAP" = Just Swap
tryParseStackOperation _ = Nothing

controlOperation :: [String]
controlOperation = ["if", "then", "else", "end", "for", "next"]

isCorrectVariableName :: String -> Bool
isCorrectVariableName = and . map (\x -> x `elem` letters)
    where
        letters = lower ++ upper
        lower = ['a'..'z']
        upper = ['A'..'Z']

tryParseVariable :: String -> [String] -> Maybe (Instruction, [String])
tryParseVariable str remain
    | str == "->" = Just (AssignVariable variableName, remain')
    | not (str `elem` controlOperation) && isCorrectVariableName str = Just (GetVariable str, remain)
    | otherwise = Nothing
    where
        variableName = head remain
        remain' = tail remain

getIfCond :: [String] -> ([String], [String])
getIfCond = getIfCond' 0
    where
        getIfCond' :: Int -> [String] -> ([String], [String])
        getIfCond' level (x:xs)
            | x == "then" && level == 0 = ([], xs)
            | otherwise = (x:fst follow, snd follow)
            where
                follow = if x == "if"
                    then getIfCond' (level+1) xs
                    else if x == "then"
                        then getIfCond' (level-1) xs
                        else getIfCond' level xs

getTrueProg :: [String] -> ([String], [String])
getTrueProg = getTrueProg' 0
    where
        getTrueProg' :: Int -> [String] -> ([String], [String])
        getTrueProg' level (x:xs)
            | x == "else" && level == 0 = ([], xs)
            | otherwise = (x:fst follow, snd follow)
            where
                follow = if x == "if"
                    then getTrueProg' (level+1) xs
                    else if x == "else"
                        then getTrueProg' (level-1) xs
                        else getTrueProg' level xs

{-tryParseIf :: [String] -> Maybe (Instruction, [String])-}
{-tryParseIf remain = Just $ (IfThenElse condProg progTrue progFalse, remain')-}
    {-where-}
        {-(condProg, remain') = parse' remain-}
        {-progTrue = []-}
        {-progFalse = []-}
tryParseIf :: [String] -> Maybe (Instruction, [String])
tryParseIf remain = Just $ (IfThenElse (internal_parse cond) (internal_parse progTrue) (internal_parse progFalse), tail remain''')
    where
        (cond, remain') = span (/= "then") remain
        (progTrue, remain'') = span (/= "else") $ drop 1 remain'
        (progFalse, remain''') = span (/= "end") $ drop 1 remain''
        internal_parse = fst . parse'

tryParseFor :: [String] -> Maybe (Instruction, [String])
tryParseFor remain = Just $ (For variableName (internal_parse progLoop), tail remain')
    where
        variableName = head remain
        (progLoop, remain') = span (/= "next") $ tail remain
        internal_parse = fst . parse'

tryParseInstr :: String -> [String] -> Maybe (Instruction, [String])
tryParseInstr str remain
    | str == "if" = tryParseIf remain
    | str == "for" = tryParseFor remain
    | otherwise = Nothing

parse' :: [String] -> (Programme, [String])
parse' [] = ([], [])
parse' (x:xs) = (instr:(fst $ parse' remain), remain)
    where
        (instr, remain) = case tryParseNumber x of
            Just number -> (Nb number, xs)
            Nothing -> case tryParseNumberOperation x of
                Just op -> (NumberOp op, xs)
                Nothing -> case tryParseBoolOperation x of
                    Just op -> (BoolOp op, xs)
                    Nothing -> case tryParseStackOperation x of
                        Just op -> (StackOp op, xs)
                        Nothing -> case tryParseVariable x xs of
                            Just (instr, remain) -> (instr, remain)
                            Nothing -> case tryParseInstr x xs of
                                Just (instr, remain) -> (instr, remain)
                                Nothing -> (Unknown x, xs)

allStackTests :: [Bool]
allStackTests = [testStack1, testStack2, testStack3]

allTests :: [Bool]
allTests = [test1, test2, test3, test4, test5, test6, test7]

main :: IO ()
main = do
    return ()
