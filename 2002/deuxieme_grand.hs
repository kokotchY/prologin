module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    _ <- getLine
    str_numbers <- getLine
    let numbers = reverse $ sort $ map read $ words str_numbers :: [Int]
    print $ numbers !! 1
    return ()
