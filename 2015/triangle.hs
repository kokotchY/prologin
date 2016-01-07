module Main where

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n :: Integer
    putStrLn $ show $ (n*(n+1)) `div` 2
