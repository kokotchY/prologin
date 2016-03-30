module Main where

import Control.Monad

count :: String -> Int
count [] = 0
count ('a':xs) = length (filter (=='b') xs) + count xs
count (_:xs) = count xs

clean :: String -> String
clean = filter (`elem` ['a','b'])

main :: IO ()
main = do
    _ <- getLine
    str <- getLine
    print $ count $ clean str
    return ()
