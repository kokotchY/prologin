module Main where

import Control.Monad

main :: IO ()
main = do
    grid <- replicateM 7 $ do
        line <- getLine
        return (map (\x -> read (x:"")) line :: [Int])
    print grid
    return ()
