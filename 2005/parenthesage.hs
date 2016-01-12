module Main where

import Control.Monad

data Info = Info
    { getP :: Int
    , getS :: Int
    , getB :: Int
    }

is_valid_parenthesage :: String -> Bool
is_valid_parenthesage sentence =
    case parse_parenthesis sentence "" of
        Right True -> True
        Left stack -> False

parse_parenthesis :: String -> String -> Either String Bool
parse_parenthesis [] [] = Right True
parse_parenthesis [] stack = Left stack
parse_parenthesis (x:xs) stack
    | x `elem` "([{" = parse_parenthesis xs (x:stack)
    | x == ')' = if (head stack) == '(' then parse_parenthesis xs (tail stack) else Left stack
    | x == ']' = if (head stack) == '[' then parse_parenthesis xs (tail stack) else Left stack
    | x == '}' = if (head stack) == '{' then parse_parenthesis xs (tail stack) else Left stack
    | otherwise = parse_parenthesis xs stack

test1, test2 :: Bool
test1 = is_valid_parenthesage "(a[b]c)" == True
test2 = is_valid_parenthesage "[a(b]c)" == False

allTests :: [Bool]
allTests = [test1, test2]

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    if is_valid_parenthesage sentence
        then print 1
        else print 0
    return ()
