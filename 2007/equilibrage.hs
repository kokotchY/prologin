module Main where

import Control.Monad
import Data.List

solution :: [Int] -> Int
solution list = fst $ minimumBy (\a b -> compare (snd a) (snd b)) res
    where
        res = zip [1..] $ map list_diff_sum $ map (flip splitAt list) [1..l]
        l = length list

list_diff_sum :: ([Int],[Int]) -> Int
list_diff_sum (a,b) = abs $ sum a - sum b

main :: IO ()
main = do
    _ <- getLine
    list <- liftM (map read . words) getLine :: IO [Int]
    print $ solution list
    return ()
