module Main where

import Control.Monad
import Data.List

compare_base :: Int -> Int -> Int -> Ordering
compare_base base a b = compare (abs $ base-a) (abs $ base-b)

closest_mul :: Int -> [Int] -> Int
closest_mul n (x:xs) = closest_mul' n (minimumBy (compare_base n) $ map ((*) x) xs) xs
    where
        closest_mul' :: Int -> Int -> [Int] -> Int
        closest_mul' _ cur [x] = cur
        closest_mul' n cur (x:xs) = closest_mul' n (min cur min_mul) xs
            where
                min_mul = minimumBy (compare_base n) $ map ((*) x) xs

main :: IO ()
main = do
    n <- readLn :: IO Int
    _ <- getLine
    list <- liftM (map read . words) getLine :: IO [Int]

    print $ closest_mul n list

    return ()
