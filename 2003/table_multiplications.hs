module Main where

import Control.Monad

table :: Int -> [String]
table = table' 1
    where
        table' :: Int -> Int -> [String]
        table' second first
            | second == 9 = [result]
            | otherwise = result : table' (second+1) first
            where
                result = show first ++ "x" ++ show second ++ "=" ++ show (first*second)


main :: IO ()
main = do
    str_nb <- getLine
    let nb = read str_nb
    mapM_ putStrLn (table nb)
    return ()
