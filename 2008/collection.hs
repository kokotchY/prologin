module Main where

import Control.Monad
import Data.List

occ :: Eq a => a -> [a] -> Int
occ a = length . filter (==a)

occ_list :: Eq a => [a] -> [(a, Int)]
occ_list list = map (\x -> (x, occ x list)) list

double :: Eq a => [a] -> a
double = fst . head . filter ((==2) . snd) . occ_list

double2 :: [Int] -> Int
double2 = double2' []
    where
        double2' :: [Int] -> [Int] -> Int
        double2' present (x:xs)
            | x `elem` present = x
            | otherwise = double2' (x:present) xs

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    numbers <- replicateM n $ do
        line <- getLine
        return $ (read line :: Int)
    print $ double2 $ sort numbers
    return ()
