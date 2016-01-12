module Main where

import Control.Monad
import Data.List

result :: String -> String
result list = left ++ right
    where
        (left, right) = foldr (\x (l, r) -> if x == 'G' then ('G':l, r) else (l, 'D':r)) ([],[]) list

main :: IO ()
main = do
    _ <- getLine
    chars <- getLine
    putStrLn $ result chars
    return ()
