module Main where

import Control.Monad
import Data.List

distance :: (Integer, Integer) -> (Integer, Integer) -> Integer
distance a b = abs $ snd a - snd b

sort_couple :: Ord a => (a,a) -> (a,a)
sort_couple (a,b) = (new_a, new_b)
    where
        new_a = min a b
        new_b = max a b

couples :: [Integer] -> [(Integer,Integer)]
couples l = nub $ map sort_couple [(x,y) | x <- l, y <- l, x /= y]

parse_numbers :: String -> [Integer]
parse_numbers = map read . words

get_couples :: [Integer] -> Integer -> Int
get_couples list k = length $ filter (\(a,b) -> abs (a-b) <= k) $ couples list

main :: IO ()
main = do
    _ <- getLine
    str_numbers <- getLine
    str_k <- getLine
    let numbers = parse_numbers str_numbers
        k = read str_k :: Integer
    print $ get_couples numbers k
    return ()
