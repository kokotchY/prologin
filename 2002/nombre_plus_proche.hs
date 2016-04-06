module Main where

import Control.Monad
import Data.List

convert :: String -> [Int]
convert = map read . words

closest :: Int -> [Int] -> Int
closest v list = minimumBy (\x y -> compare (calc x) (calc y)) list
    where
        calc :: Int -> Int
        calc x = abs $ x - v

main :: IO ()
main = do
    _ <- getLine
    str_list1 <- getLine
    _ <- getLine
    str_list2 <- getLine
    let list1 = convert str_list1
        list2 = convert str_list2
        list3 = map (flip closest list1) list2
    putStrLn $ unwords $ map show list3
    return ()
