module Main where

import Control.Monad

type Grid = [[Int]]

is_safe_detonate :: (Int, Int) -> (Int, Int) -> Grid -> Bool
is_safe_detonate (bx,by) (px,py) grid = undefined

detonate_bomb :: (Int, Int) -> Grid -> (Grid, [(Int, Int)])
detonate_bomb (bx, by) grid = undefined
    where
        touch_bomb = filter ((>0) . flip get_bomb_radius grid) $ affected_cell (bx,by) radius
        radius = get_bomb_radius (bx,by) grid

affected_cell :: (Int, Int) -> Int -> [(Int,Int)]
affected_cell (bx,by) r = along_x ++ along_y
    where
        along_x = [(x,y) | x <- [(bx-r)..(bx+r)], y <- [by], not (x == bx && y == by)]
        along_y = [(x,y) | x <- [bx], y <- [(by-r)..(by+r)], not (x == bx && y == by)]

get_bomb_radius :: (Int, Int) -> Grid -> Int
get_bomb_radius (bx,by) grid = value
    where
        value = if bound_x && bound_y
            then grid !! by !! bx
            else 0
        bound_y = by >= 0 && by < (length grid)
        bound_x = bx >= 0 && bx < (length $ head grid)

grid :: Grid
grid = [[0,0,0],[0,4,0],[0,0,0]]

main :: IO ()
main = do
    str_size <- getLine
    str_pos <- getLine
    str_init_pos <- getLine
    let [h, l] = map read $ words str_size :: [Int]
        [px, py] = map read $ words str_pos :: [Int]
        [bx, by] = map read $ words str_init_pos :: [Int]
    grid <- replicateM h $ do
        line <- getLine
        let values = map read $ words line :: [Int]
        return values
    return ()
