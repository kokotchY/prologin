module Main where

import Control.Monad

eval :: String -> Int

main :: IO ()
main = do
    expr <- getLine
    print $ eval $ words expr
    return ()
