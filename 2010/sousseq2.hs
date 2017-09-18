module Main where

import Control.Monad
import Data.List
import qualified Data.Map.Lazy as Map

type Occ = Map.Map String Int

max_occ :: (String, Int) -> (String, Int) -> Ordering
max_occ (k1, occ1) (k2, occ2)
    | occ1 == occ2 =
        case compare k1 k2 of
            LT -> GT
            GT -> LT
    | otherwise = compare occ1 occ2

increment :: String -> Occ -> Occ
increment key occ =
    case Map.lookup key occ of
        Just v -> Map.insert key (v+1) occ
        Nothing -> Map.insert key 1 occ

count :: String -> Int -> Occ
count = count' Map.empty
    where
        count' :: Occ -> String -> Int -> Occ
        count' res str l
            | length str < l = res
            | otherwise = count' res' (tail str) l
            where
                res' = increment (take l str) res

main :: IO ()
main = do
    _ <- getLine
    l <- readLn :: IO Int
    line <- getLine
    let (key, c) = maximumBy max_occ $ Map.toList $ count line l
    putStrLn key
    return ()
