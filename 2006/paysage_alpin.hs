module Main where

import Control.Monad
import Data.List

data1 :: [String]
data1 = [
    "   <         > <    >     ",
    "  < >   <   > < >  < >    ",
    " <   ><> < >     <>   < > ",
    "<         >            < >"]

fromJust :: Maybe a -> a
fromJust (Just a) = a

is_pic :: Int -> Int -> Int -> Bool
is_pic x y z = x == y - 1 && z == y - 1

count_pic :: [Int] -> Int
count_pic heights
    | length heights < 3 = 0
    | is_pic x y z = 1 + count_pic (y:z:xs)
    | otherwise = count_pic (y:z:xs)
    where
        (x:y:z:xs) = heights

get_pic :: [Int] -> [(Int,Int,Int)]
get_pic heights
    | length heights < 3 = []
    | is_pic x y z = (x,y,z):get_pic (y:z:xs)
    | otherwise = get_pic (y:z:xs)
    where
        (x:y:z:xs) = heights

get_height :: [String] -> [Int]
get_height = map (fromJust . findIndex ((/=) ' ') . reverse) . transpose

main :: IO ()
main = do
    [h, l] <- liftM (map read . words) getLine :: IO [Int]
    montains <- replicateM h getLine :: IO [String]
    print $ count_pic $ get_height montains
    return ()
