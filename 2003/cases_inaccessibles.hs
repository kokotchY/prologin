module Main where

import Control.Monad
import qualified Data.Map.Strict as Map

type Pos = (Integer, Integer)

data Cell = C
    { getX :: Integer
    , getY :: Integer
    , value :: Integer
    , visited :: Bool
    }
    deriving Show

type Grid = Map.Map Pos Cell

get_accessibles :: Grid -> Pos -> Grid
get_accessibles grid pos = result_grid
    where
        modified_grid = set_visited pos grid
        cell = get_cell modified_grid pos
        (x, y) = get_pos cell
        positions = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        cells = filter (\x -> value x <= get_value cell && not (visited x)) $ get_cells positions modified_grid
        result_grid = foldl do_visit modified_grid cells

do_visit :: Grid -> Cell -> Grid
do_visit grid cell = get_accessibles grid (get_pos (Just cell))

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
get_cell grid pos = Map.lookup pos grid

create_cell :: Integer -> (Integer, Integer) -> (Pos, Cell)
create_cell y (x,v) = ((x,y), C x y v False)

get_grid :: Integer -> Integer -> IO Grid
get_grid l c = do
    grid <- get_grid' l c 0
    return $ Map.fromList grid
    where
        get_grid' :: Integer -> Integer -> Integer -> IO [(Pos,Cell)]
        get_grid' l c idx
            | l == idx = return []
            | otherwise = do
                line <- getLine
                let nb = zip [0..] (map read $ words line :: [Integer])
                elements <- get_grid' l c (idx+1)
                return $ (map (create_cell idx) nb) ++ elements

set_visited :: Pos -> Grid -> Grid
set_visited = Map.adjust (\c -> c { visited = True })

main :: IO ()
main = do
    str_params <- getLine
    let [l, c] = map read $ words str_params :: [Integer]
    grid <- get_grid l c
    print $ Map.size $ Map.filter (not.visited) $ get_accessibles grid (0,0)
    return ()
