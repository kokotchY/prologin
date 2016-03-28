module Main where

import Control.Monad

type Pos = (Integer, Integer)

data Cell = C
    { getX :: Integer
    , getY :: Integer
    , value :: Integer
    , visited :: Bool
    }
    deriving Show

type Grid = [(Pos, Cell)]

get_accessibles :: Grid -> Pos -> [Cell]
get_accessibles grid pos = cells
    where
        cell = get_cell grid pos
        (x, y) = get_pos cell
        positions = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        cells = filter (\x -> value x <= get_value cell) $ get_cells positions grid

get_value :: Maybe Cell -> Integer
get_value Nothing = -999999
get_value (Just cell) = value cell

get_pos :: Maybe Cell -> Pos
get_pos Nothing = (-1,-1)
get_pos (Just cell) = (getX cell, getY cell)

get_cells :: [Pos] -> Grid -> [Cell]
get_cells positions grid = get_justs $ foldl (\x y -> x ++ [get_cell grid y]) [] positions

get_justs :: [Maybe Cell] -> [Cell]
get_justs [] = []
get_justs (Nothing:xs) = get_justs xs
get_justs (Just cell:xs) = cell:get_justs xs


get_cell :: Grid -> Pos -> Maybe Cell
get_cell grid pos = lookup pos grid

create_cell :: Integer -> (Integer, Integer) -> (Pos, Cell)
create_cell y (x,v) = ((x,y), C x y v False)

get_grid :: Integer -> Integer -> IO Grid
get_grid l c = get_grid' l c 0
    where
        get_grid' :: Integer -> Integer -> Integer -> IO Grid
        get_grid' l c idx
            | l == idx = return []
            | otherwise = do
                line <- getLine
                let nb = zip [0..] (map read $ words line :: [Integer])
                elements <- get_grid' l c (idx+1)
                return $ (map (create_cell idx) nb) ++ elements

main :: IO ()
main = do
    str_params <- getLine
    let [l, c] = map read $ words str_params :: [Integer]
    grid <- get_grid l c
    print $ get_accessibles grid (0,0)
    return ()
