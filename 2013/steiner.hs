module Main where

import Control.Monad
import Data.List

occ :: Eq a => a -> [a] -> Int
occ nb = length . filter (==nb)

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params :: [Int]
        n = params !! 0
        m = params !! 1
    list <- replicateM m $ do
        line <- getLine
        return $ filter (/= '.') line
    let list2 = concat list
    let list_player = nub list2
    let list_occ = map (\x -> (x, occ x list2)) list_player
    let sorted_list = sortBy (\x y -> compare (snd y) (snd x)) list_occ
    let sorted_player_list = map fst sorted_list
    putStrLn sorted_player_list
