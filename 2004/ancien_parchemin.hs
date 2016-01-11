module Main where

import Control.Monad

equal :: String -> String -> Bool
equal [] [] = True
equal (x:xs) (y:ys)
    | x == y = equal xs ys
    | x /= y && x == '?' = equal xs ys
    | otherwise = False

main :: IO ()
main = do
    _ <- getLine
    parchemin <- getLine
    _ <- getLine
    assistant <- getLine
    if length parchemin /= length assistant
        then putStrLn "0"
        else if equal parchemin assistant
            then putStrLn "1"
            else putStrLn "0"


    return ()
