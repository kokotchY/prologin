module Main where

import Control.Monad

power_2 :: [Int]
power_2 = map ((^) 2) [0..]

main :: IO ()
main = do
    m <- readLn :: IO Int
    let list = dropWhile (\n -> n <= m) power_2
    print $ head list
    return ()
