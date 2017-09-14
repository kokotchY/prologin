module Main where

import Control.Monad

is_possible :: Int -> [Int] -> Bool
is_possible 0 _ = True
is_possible _ [] = False
is_possible w (x:xs)
    | w - x == 0 = True
    | otherwise = is_possible w xs || is_possible (w-x) xs

main :: IO ()
main = do
    [n, p] <- liftM (map read . words) getLine :: IO [Int]
    weights <- liftM (map read . words) getLine :: IO [Int]
    if is_possible p weights
        then putStrLn "OUI"
        else putStrLn "NON"
    return ()
