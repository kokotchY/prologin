module Main where

import Control.Monad
import Data.List

convert :: String -> [Int]
convert = map (read . (flip (:) ""))

count :: (Int -> Bool) -> [Int] -> Int
count _ [] = 0
count f (x:xs) = v + count f xs
    where
        v = if f x
            then 1
            else 0

calc :: ([Int],[Int]) -> Int
calc (left, right) = count (==1) left + count (==0) right

min_sum :: (Int,Int) -> (Int,Int) -> Ordering
min_sum c1 c2 = compare (snd c1) (snd c2)

data1 :: [Int]
data1 = convert "001101011000010110110011101001101010101010"

main :: IO ()
main = do
    _ <- getLine
    list <- liftM (map (read . (flip (:) ""))) getLine :: IO [Int]
    let all_pos = [0..length data1]
        solution = minimumBy min_sum $ map (\p -> (p, calc $ splitAt p list)) all_pos
    print $ fst solution
    return ()
