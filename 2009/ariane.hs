module Main where

import Control.Monad
import qualified Data.Map.Lazy as Map
import Data.List

type Pos = (Integer, Integer)
type Grid = Map.Map Pos Cell

data Cell = C
    { getX :: Integer
    , getY :: Integer
    , getValue :: Char
    , visited :: Bool
    }
    deriving Show

convert_pos :: String -> Pos
convert_pos pos = (x,y)
    where
        [x,y] = map read $ words pos

convert_grid :: [String] -> Grid
convert_grid = Map.fromList . concatMap (\(l, line) -> zipWith (\c char -> ((c,l),C c l char False)) [0..] line) . zip [0..]

emptyCell :: Cell
emptyCell = C (-1) (-1) 'X' True

get_cell :: Pos -> Grid -> Cell
get_cell pos grid =
    case Map.lookup pos grid of
        Just cell -> cell
        Nothing -> emptyCell

set_visited :: Pos -> Grid -> Grid
set_visited = Map.adjust (\c -> c { visited = True })

visit_grid :: Pos -> Grid -> Grid
visit_grid pos grid = grid''
    where
        grid' = set_visited pos grid
        cells = filter ((=='.') . getValue) $ get_cells (next_position pos) grid'
        grid'' = foldl' (\g c -> visit_grid (get_pos c) g) grid' cells

get_pos :: Cell -> Pos
get_pos c = (getX c,getY c)

next_position :: Pos -> [Pos]
next_position (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

get_cells :: [Pos] -> Grid -> [Cell]
get_cells pos grid = filter (not . visited) $ map (flip get_cell grid) pos

test_grid :: [String]
test_grid = ["...X","XX..","....",".XXX"]

main :: IO ()
main = do
    str_params <- replicateM 6 getLine
    let h = read $ head str_params
        [l, tty_y, tty_x, food_y, food_x] = map read $ tail str_params :: [Integer]
        tty = (tty_x, tty_y)
        food = (food_x, food_y)
    str_grid <- replicateM h getLine
    let grid = convert_grid str_grid
        grid' = visit_grid tty grid
    if visited (get_cell food grid')
        then putStrLn "1"
        else putStrLn "0"
    return ()
