module Main where

import Control.Monad

neededBallons :: [Int] -> Int
neededBallons list = neededBallons' 0 list
    where
        neededBallons' :: Int -> [Int] -> Int
        neededBallons' remain [] = 0
        neededBallons' remain (x:xs)
            | x <= remain = neededBallons' (remain-x + x `div` 2) xs
            | x > remain = (x-remain) + neededBallons' (x `div` 2) xs

doTest1 :: Bool
doTest1 = neededBallons [20, 20] == 30

doTest2 :: Bool
doTest2 = neededBallons [10, 20, 30, 40] == 70

doTest3 :: Bool
doTest3 = neededBallons [80, 10, 80] == 125

allTests :: [Bool]
allTests = [doTest1, doTest2, doTest3]

main :: IO ()
main = do
    _ <- getLine
    str_b <- getLine
    let b = map read $ words str_b :: [Int]
    print $ neededBallons b
    return ()
