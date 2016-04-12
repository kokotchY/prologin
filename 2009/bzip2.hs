module Main where

import Control.Monad

main :: IO ()
main = do
    str_n <- getLine
    sentence <- getLine
    str_no <- getLine
    let n = read str_n
        no = read str_no
    return ()
