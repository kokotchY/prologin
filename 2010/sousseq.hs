module Main where

import Control.Monad
import Data.List (nub, sort, sortBy)

occ :: Eq a => a -> [a] -> Int
occ a = length . filter (==a)

sequences :: Int -> String -> [String]
sequences nb list
    | length list >= nb = take nb list : sequences nb (drop 1 list)
    | otherwise = []

most_occ :: Int -> String -> String
most_occ nb list = fst $ head $ sortBy (\x y -> compare (snd y) (snd x)) $ map (\x -> (x, occ x s)) $ nub s2
    where
        s = sequences nb list
        s2 = sort s

main :: IO ()
main = do
    _ <- getLine
    str_l <- getLine
    line <- getLine
    let l = read str_l :: Int
    putStrLn $ most_occ l line
    return ()
