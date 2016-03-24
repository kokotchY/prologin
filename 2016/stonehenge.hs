module Main where

import Control.Monad

solution :: String -> Int
solution list = length $ filter (\(a,b) -> a /= b) $ zip list (reverse list)

main :: IO ()
main = do
    _ <- getLine
    plan <- getLine
    print $ solution $ tail plan
    return ()
