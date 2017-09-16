module Main where

import Control.Monad

rotation :: String -> Int -> String
rotation str k = take l $ drop (l-k) str'
    where
        l = length str
        str' = cycle str

main :: IO ()
main = do
    _ <- getLine
    str <- getLine
    rot <- readLn
    putStrLn $ rotation str rot
    return ()
