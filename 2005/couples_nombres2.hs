
module Main where

import Control.Monad

count :: (a -> Bool) -> [a] -> Int
count = count' 0
    where
        count' :: Int -> (a -> Bool) -> [a] -> Int
        count' tot _ [] = tot
        count' tot f (x:xs)
            | f x = count' (1+tot) f xs
            | otherwise = count' tot f xs

count2 :: (a -> Bool) -> [a] -> Int
count2 f = foldl (\t v -> if f v then (t+1) else t) 0

min_dist :: Int -> (Int, Int) -> Bool
min_dist k (v1,v2) = abs (v1 - v2) <= k

couples :: [Int] -> [(Int,Int)]
couples = couples' []
    where
        couples' :: [(Int,Int)] -> [Int] -> [(Int,Int)]
        couples' res [] = res
        couples' res (x:xs) = couples' res' xs
            where
                res' = map ((,) x) xs ++ res

main :: IO ()
main = do
    _ <- getLine
    list <- liftM (map read . words) getLine :: IO [Int]
    k <- readLn :: IO Int
    print $ count (min_dist k) $ couples list
    return ()
