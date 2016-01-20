module Main where

import RPL

main :: IO ()
main = do
    str_prog <- getLine 
    let prog = parse str_prog
        result = execResult prog
    putStrLn result
