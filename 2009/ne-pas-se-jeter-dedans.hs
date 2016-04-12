
module Main where

import Control.Monad
import Data.List

type Pos = (Integer, Integer)

type Grid = [(Pos, Bool)]

getPos :: IO Pos
getPos = do
    line <- getLine
    let [y, x] = map read $ words line
    return $ (x,y)

test_grid :: [[Bool]]
test_grid = [[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,True,False,False,False,False,False],[False,False,False,True,True,True,True,False,False,False],[False,False,False,False,True,True,True,True,False,False],[False,False,False,False,False,False,True,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False],[False,False,False,False,False,False,False,False,False,False]]


coeff_dir :: Pos -> Pos -> Integer
coeff_dir (x1, y1) (x2, y2) = (y2 - y1) `div` (x2 - x1)

get_fuel_area :: [[Bool]] -> (Pos, Pos)
get_fuel_area grid = (top_left, bottom_right)
    where
        grid' = dropWhile (\(idx, arr) -> all not arr) $ zip [0..] grid
        (y1, y2) = map_couple (fst.head) ((\x -> x-1) . fst.head) $ span (or . snd) grid'
        grid_x = transpose grid
        grid_x' = dropWhile (\(idx, arr) -> all not arr) $ zip [0..] grid_x
        (x1, x2) = map_couple (fst.head) ((\x -> x-1) . fst.head) $ span (or . snd) grid_x'
        top_left = (x1, y1)
        bottom_right = (x2, y2)

first_line :: [[Bool]] -> Integer
first_line = fst . head . dropWhile (\(idx, arr) -> all not arr) . zip [0..]

map_couple :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
map_couple f g (a,b) = (f a, g b)

main :: IO ()
main = do
    str_n <- getLine
    str_m <- getLine
    let n = read str_n :: Int
        m = read str_m :: Integer
    grid <- replicateM n $ do
        line <- getLine
        let nb = map (=="1") $ words line
        return nb
    pos_simon <- getPos
    pos_car <- getPos
    print grid
    return ()

{-  0 1 2 3 4 5 6 7 8 9-}
{-0 0 0 0 0 0 0 0 0 0 0-}
{-1 0 0 0 0 0 0 0 0 0 0-}
{-2 0 0 0 0 0 0 0 0 0 0-}
{-3 0 0 0 0 1 0 0 0 0 0-}
{-4 0 0 0 1 1 1 1 0 0 0-}
{-5 0 0 0 0 1 1 1 1 0 0-}
{-6 0 0 0 0 0 0 1 0 0 0-}
{-7 0 0 0 0 0 0 0 0 0 0-}
{-8 0 0 0 0 0 0 0 0 0 0-}
{-9 0 0 0 0 0 0 0 0 0 0-}
{-dropWhile (\(idx, arr) -> all not arr) $ zip [0..] test_grid-}
{-span (or . snd) s-}
