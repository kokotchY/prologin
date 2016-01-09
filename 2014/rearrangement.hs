module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    _ <- getLine
    str_first <- getLine
    str_second <- getLine
    let first = map read $ words str_first :: [Int]
        second = map read $ words str_second :: [Int]
    if sort first == sort second
        then print 1
        else print 0
    return ()
