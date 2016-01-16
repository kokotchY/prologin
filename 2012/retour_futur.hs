module Main where

import Control.Monad
import Data.List

gen :: [Int] -> [Int]
gen [] = []
gen (x:y:xs) = [x..y] ++ gen xs

most_occ :: [Int] -> Int
most_occ = length . maximumBy (\a b -> compare (length a) (length b)) . group . sort

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    intervals <- replicateM n $ do
        line <- getLine
        let [start,end] = map read $ words line :: [Int]
        return $ [start..end]
    print $ most_occ $ concat intervals
    return ()
