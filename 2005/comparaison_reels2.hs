module Main where

import Control.Monad
import Data.List

compare' :: (Int, Int) -> (Int, Int) -> Ordering
compare' (i1, d1) (i2, d2)
    | i == EQ = compare d1 d2
    | otherwise = i
    where
        i = compare i1 i2

display :: Ordering -> Int
display LT = -1
display EQ = 0
display GT = 1

read' :: String -> Int
read' "" = 0
read' str = read str

convert :: String -> (Int, Int)
convert nb = (int', decimal')
    where
        (int, decimal) = break ((==) '.') nb
        int' = read' int
        decimal' = if length decimal > 0
            then read' $ reverse $ dropWhile ((==) '0') $ reverse $ tail decimal
            else 0

main :: IO ()
main = do
    _ <- getLine
    number1 <- getLine
    _ <- getLine
    number2 <- getLine
    print $ display $ compare' (convert number1) (convert number2)
    return ()
