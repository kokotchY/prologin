module Main where

import Control.Monad

is_possible :: String -> String -> Bool
is_possible [] (x:xs) = False
is_possible _ [] = True
is_possible (x:[]) (y:[]) = x == y
is_possible (x:xs) (y:ys)
    | x == y = is_possible xs ys
    | otherwise = is_possible xs (y:ys)

main :: IO ()
main = do
    _ <- getLine
    message <- getLine
    _ <- getLine
    test_message <- getLine
    if is_possible message test_message
        then putStrLn "1"
        else putStrLn "0"
    return ()
