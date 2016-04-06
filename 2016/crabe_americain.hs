module Main where

import Control.Monad

type Pos = (Integer, Integer)

distance :: Pos -> Pos -> Integer
distance (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

convert_grid :: [String] -> [(Integer, Integer, Char)]
convert_grid = concatMap convert_line . zip [0..]

convert_line :: (Integer, [Char]) -> [(Integer, Integer, Char)]
convert_line (y, line) = map (\(x, c) -> (x, y, c)) $ zip [0..] line

get_pos :: [String] -> (Pos, Pos)
get_pos grid = (pos1, pos2)
    where
        [pos1, pos2] = take 2 elements
        elements = map (\(x,y,c) -> (x,y)) $ filter ((/=) '.' . thr) converted_grid
        converted_grid = convert_grid grid

thr :: (a,b,c) -> c
thr (_,_,v) = v

solution :: [String] -> Integer
solution grid = distance (fst result) (snd result)
    where
        result = get_pos grid

main :: IO ()
main = do
    str_params <- getLine
    let [m,n] = map read $ words str_params :: [Int]
    grid <- replicateM m getLine
    print $ solution grid
    return ()
