module Main where

syracuse :: Integer -> Integer -> Integer
syracuse start k
    | k == 0 = start
    | odd previous = previous * 3 + 1
    | even previous = previous `div` 2
    where
        previous = syracuse start (k-1)

main :: IO ()
main = do
    str_u <- getLine
    str_k <- getLine
    let u = read str_u :: Integer
        k = read str_k :: Integer
    putStrLn $ show $ syracuse u k
