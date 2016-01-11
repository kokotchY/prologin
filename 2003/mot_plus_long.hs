module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    _ <- getLine
    str <- getLine
    let sentence = words str
        size = sortBy (\x y -> compare (length x) (length y)) sentence
        result = length $ last size
    print result
    return ()
