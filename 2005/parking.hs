module Main where

import Control.Monad
import Data.Map.Lazy as Map
import qualified Data.List as L

data Cell = C
    { getX :: Integer
    , getY :: Integer
    , value :: Integer
    , visited :: Bool
    }
    deriving Show

type Pos = (Integer,Integer)

data Grid = G
    { get_parking :: Map.Map Pos Cell
    , free_space :: Integer
    }
    deriving Show

create_cell :: Integer -> (Integer, Char) -> (Pos, Cell)
create_cell y (x, c) = ((x,y), C x y v False)
    where
        v = read (c:"")

get_grid :: Integer -> Integer -> IO Grid
get_grid w h = get_grid' w h 0
    where
        get_grid' :: Integer -> Integer -> Integer -> IO Grid
        get_grid' w h idx
            | idx == h = return $ G (Map.fromList []) 0
            | otherwise = do
                line <- getLine
                others <- get_grid' w h (idx+1)
                let list = zip [0..] line
                return $ G (Map.union (fromList (Prelude.map (create_cell idx) list)) (get_parking others)) 0

count_places :: Grid -> Pos -> Grid
count_places grid pos = grid''
    where
        grid' = mark_visited grid pos
        cell = get_cell' grid' pos
        x = getX cell
        y = getY cell
        positions = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
        cells = Prelude.filter (\c -> not (visited c) && value c /= 3 ) $ get_cells positions grid'
        grid'' = L.foldl' (visit) grid' cells

visit :: Grid -> Cell -> Grid
visit grid cell = count_places grid (get_pos cell)

valid_parking :: Cell -> Bool
valid_parking cell = visited cell && value cell == 1

get_pos :: Cell -> Pos
get_pos cell = (getX cell, getY cell)

fakeCell :: Cell
fakeCell = C (-1) (-1) (-1) False

fromJust :: Maybe a -> a
fromJust (Just a) = a

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

get_cells :: [Pos] -> Grid -> [Cell]
get_cells positions grid = Prelude.map fromJust $ Prelude.filter isJust $ Prelude.map (get_cell grid) positions

get_cell :: Grid -> Pos -> Maybe Cell
get_cell grid pos = Map.lookup pos (get_parking grid)

get_cell' :: Grid -> Pos -> Cell
get_cell' grid pos =
    case Map.lookup pos (get_parking grid) of
        Just cell -> cell
        Nothing -> fakeCell

mark_visited :: Grid -> Pos -> Grid
mark_visited grid pos = grid { get_parking = parking }
    where
        parking = Map.adjust (\x -> x { visited = True }) pos (get_parking grid)

nb_free_parking :: Grid -> Int
nb_free_parking = Map.size . Map.filter valid_parking . get_parking . flip count_places (0,0)

main :: IO ()
main = do
    str_params <- getLine
    let [w,h] = Prelude.map read $ words str_params :: [Integer]
    grid <- get_grid w h
    print $ nb_free_parking grid
    return ()
