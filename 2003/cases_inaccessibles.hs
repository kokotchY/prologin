module Main where

import Control.Monad
import Data.Foldable (foldl')
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
        result_grid = foldl' do_visit modified_grid cells

do_visit :: Grid -> Cell -> Grid
do_visit grid cell = get_accessibles grid (get_pos (Just cell))

get_value :: Maybe Cell -> Integer
get_value Nothing = -999999
get_value (Just cell) = value cell

get_pos :: Maybe Cell -> Pos
get_pos Nothing = (-1,-1)
get_pos (Just cell) = (getX cell, getY cell)

get_cells :: [Pos] -> Grid -> [Cell]
get_cells positions grid = get_justs $ foldl' (\x y -> x ++ [get_cell grid y]) [] positions

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

test_grid :: Grid
test_grid = Map.fromList [((0,0),C {getX = 0, getY = 0, value = 4, visited = False}),((0,1),C {getX = 0, getY = 1, value = 3, visited = False}),((0,2),C {getX = 0, getY = 2, value = 4, visited = False}),((0,3),C {getX = 0, getY = 3, value = 0, visited = False}),((1,0),C {getX = 1, getY = 0, value = 5, visited = False}),((1,1),C {getX = 1, getY = 1, value = 2, visited = False}),((1,2),C {getX = 1, getY = 2, value = 1, visited = False}),((1,3),C {getX = 1, getY = 3, value = 1, visited = False}),((2,0),C {getX = 2, getY = 0, value = 3, visited = False}),((2,1),C {getX = 2, getY = 1, value = 6, visited = False}),((2,2),C {getX = 2, getY = 2, value = 1, visited = False}),((2,3),C {getX = 2, getY = 3, value = 2, visited = False})]

main :: IO ()
main = do
    str_params <- getLine
    let [l, c] = map read $ words str_params :: [Integer]
    grid <- get_grid l c
    print $ Map.size $ Map.filter (not.visited) $ get_accessibles grid (0,0)
    return ()
