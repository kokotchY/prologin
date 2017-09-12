module Main where

import Control.Monad

calc_clockwise :: Int -> Int -> [Int] -> Int
calc_clockwise s t cost
    | s == t = 0
    | otherwise = value + calc_clockwise s' t cost
    where
        value = if s > length cost
            then cost !! 0
            else cost !! (s-1)
        s' = if (s+1) > length cost
            then 1
            else s+1

calc_counter_clockwise :: Int -> Int -> [Int] -> Int
calc_counter_clockwise s t cost
    | s == t = 0
    | otherwise = value + calc_counter_clockwise s' t cost
    where
        value = if s == 1
            then cost !! (length cost - 1)
            else cost !! (s-2)
        s' = if (s-1) == 0
            then length cost
            else s-1

test1, test2, test3 :: (Int, Int)
test1 = (calc_clockwise 1 3 cost, calc_counter_clockwise 1 3 cost)
    where
        cost = [2,3,2,1]
test2 = (calc_clockwise 1 1 cost, calc_counter_clockwise 1 1 cost)
    where
        cost = [5,3,4]
test3 = (calc_clockwise 3 1 cost, calc_counter_clockwise 3 1 cost)
    where
        cost = [3,2,5,1]

main :: IO ()
main = do
    [n,s,t] <- liftM (map read . words) getLine :: IO [Int]
    distances <- replicateM n (liftM read getLine) :: IO [Int]
    let clockwise = calc_clockwise s t distances
        counter_clockwise = calc_counter_clockwise s t distances
    print $ min clockwise counter_clockwise
    return ()
