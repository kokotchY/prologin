module Main where

import Control.Monad

get_elem :: [String] -> (Int,Int) -> Char
get_elem grid (x,y) = grid !! y !! x

nb_wall :: String -> Int
nb_wall = length . filter (=='x')

nb_secret_room :: [String] -> Int
nb_secret_room room = length $ filter (>=3) $ map (flip get_nb_wall room) all_pos
    where
        all_pos = [(x,y) | x <- [0..size], y <- [0..size]]
        size = length room - 1
        get_nb_wall :: (Int, Int) -> [String] -> Int
        get_nb_wall pos@(x,y) room
            | get_elem room pos == 'o' = nb_wall (get_room x y room)
            | otherwise = 0

get_room :: Int -> Int -> [String] -> String
get_room x y grid = map (get_elem grid) (get_room_pos (length grid) (x,y))

get_room_pos :: Int -> (Int, Int) -> [(Int, Int)]
get_room_pos size (x,y) = filter all_pos [(x, y-1), (x+1, y), (x, y+1), (x-1, y)]
    where
        all_pos :: (Int,Int) -> Bool
        all_pos (x,y) = x >= 0 && y >= 0 && x < size && y < size

room1 :: [String]
room1 = ["ooxoo", "oxxxo", "oxxxo", "oxoxo", "ooooo"]

room2 :: [String]
room2 = ["ooxoo", "oxxxo", "oxxxo", "oxoxo", "oxxxo"]

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    grid <- replicateM n getLine
    print $ nb_secret_room grid
    return ()
