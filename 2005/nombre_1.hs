module Main where

import Control.Monad
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

bin :: Int -> String
bin nb = showIntAtBase 2 intToDigit nb ""

result :: Int -> Int
result = length . filter (=='1') . bin

main :: IO ()
main = do
    str_n <- getLine
    print $ result $ read str_n
    return ()
