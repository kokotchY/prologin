module Main where

import Control.Monad

distance :: Player -> Player -> Int
distance p1 p2
    | getX p1 == getX p2 = abs $ getY p1 - getY p2
    | getY p1 == getY p2 = abs $ getX p1 - getX p2

can_throw :: Player -> Player -> Bool
can_throw p1 p2 = getA p1 >= distance p1 p2

do_test :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
do_test x1 y1 a1 x2 y2 a2 = can_throw (Player x1 y1 a1) (Player x2 y2 a2)

is_possible :: [Player] -> Bool
is_possible players = is_possible' (players ++ [head players])
    where
        is_possible' :: [Player] -> Bool
        is_possible' [] = True
        is_possible' (p:[]) = True
        is_possible' (p1:p2:ps)
            | can_throw p1 p2 = is_possible' (p2:ps)
            | otherwise = False

data Player = Player
    { getX :: Int
    , getY :: Int
    , getA :: Int
    }

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    players <- replicateM n $ do
        line <- getLine
        let [x,y,a] = map read $ words line
        return $ Player x y a
    if is_possible players
        then print 1
        else print 0
    return ()
