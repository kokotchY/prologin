module Main where

import Control.Monad

main :: IO ()
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ abs $ a-b
    return ()
