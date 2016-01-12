module Main where

import Control.Monad

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    putStrLn $ reverse sentence
    return ()
