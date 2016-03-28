module Main where

import Control.Monad
import Data.List

convert :: String -> [Int]
convert [] = []
convert (x:xs) = (read (x:[])) : convert xs

calc :: ([Int], [Int]) -> Int
calc (l1, l2) = v_l1 + v_l2
    where
        v_l1 = length $ filter (==1) l1
        v_l2 = length $ filter (==0) l2

solution :: [Int] -> Int
solution l = fst $ minimumBy (\c1 c2 -> compare (snd c1) (snd c2)) $ map f [0..length l]
    where
        f :: Int -> (Int, Int)
        f = \x -> (x, calc $ splitAt x list)

list :: [Int]
list = [0,0,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,0,1,1,0,0,1,1,1,0,1,0,0,1,1,0,1,0,1,0,1,0,1,0,1,0]

main :: IO ()
main = do
    _ <- getLine
    str_list <- getLine
    let list = convert str_list
    print $ solution list
    return ()
