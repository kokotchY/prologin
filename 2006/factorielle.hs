module Main where

import Control.Monad
import Data.List

factorielle :: Integer -> Integer
factorielle n = foldl' (*) 1 [1..n]

main :: IO ()
main = do
    str_nb <- getLine
    let nb = read str_nb
    print $ factorielle nb
    return ()
