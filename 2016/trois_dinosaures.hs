module Main where

import Control.Monad

play :: Char -> Char -> Int
play p1 p2
    | p1 == p2 = 0
    | p1 == 'C' && p2 == 'A' = 1
    | p2 == 'C' && p1 == 'A' = 2
    | p1 < p2 = 1
    | p2 < p1 = 2

main :: IO ()
main = do
    str_p1 <- getLine
    str_p2 <- getLine
    let p1 = head str_p1
        p2 = head str_p2
    print $ play p1 p2
    return ()
