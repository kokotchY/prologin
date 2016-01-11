module Main where

import Control.Monad
import Data.Char

main :: IO ()
main = do
    _ <- getLine
    str <- getLine
    let sentence = words $ map toUpper str
        result = map head sentence
    putStrLn result
    return ()
