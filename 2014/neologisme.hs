{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad

main :: IO ()
main = do
    getLine
    word <- getLine
    str_n <- getLine
    let n = read str_n
    !dictionary <- replicateM n $ getLine >> getLine
    if word `elem` dictionary
        then print 0
        else print 1
    return ()
