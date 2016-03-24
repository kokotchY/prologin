module Main where

import Control.Monad

type Pos = (Integer,Integer)

data Dyno = D
    { get_x :: Integer
    , get_y :: Integer
    , meteorites :: [Pos]
    , nb_meteorite :: Integer
    , is_alive :: Bool
    }
    deriving Show

dyno :: Pos -> Dyno
dyno (x,y) = D x y [] 0 True

move_dyno :: Dyno -> Pos -> Dyno
move_dyno dyno (x,y)
    | get_x dyno == x && get_y dyno == y = dyno { is_alive = False }
    | (new_x, new_y) `elem` m = dyno { get_x = new_x, get_y = new_y, is_alive = False, meteorites = (x,y):m }
    | otherwise = dyno { get_x = new_x, get_y = new_y, meteorites = (x,y):m, nb_meteorite = nb_meteorite dyno + 1 }
    where
        (new_x, new_y) = calc_new_pos (get_x dyno, get_y dyno) (x,y)
        m = meteorites dyno

calc_new_pos :: Pos -> Pos -> Pos
calc_new_pos (x1,y1) (x2,y2)
    | x1 == x2 && y1 == y2 = (x1,y1)
    | x1 == x2 && y1 > y2 = (x1,y1+1)
    | x1 < x2 && y1 > y2 = (x1-1,y1+1)
    | y1 == y2 && x1 < x2 = (x1-1,y1)
    | x1 < x2 && y1 < y2 = (x1-1,y1-1)
    | x1 == x2 && y1 < y2 = (x1,y1-1)
    | y1 == y2 && x1 > x2 = (x1+1,y1)
    | x1 > x2 && y1 > y2 = (x1+1,y1+1)
    | otherwise = error "Impossible"

main :: IO ()
main = do
    str_n <- getLine
    str_pos <- getLine
    let n = read str_n
        [x,y] = map read $ words str_pos :: [Integer]
    list <- replicateM n $ do
        line <- getLine
        let [i,j] = map read $ words line :: [Integer]
        return (i,j)
    let last_dyno = foldl move_dyno (dyno (x,y)) list
    print $ nb_meteorite last_dyno
    return ()
