module Main where

import Control.Monad

data Cell = C
    { getX :: Integer
    , getY :: Integer
    , value :: Char
    , ocean :: Bool
    }
    deriving Show

type Pos = (Integer,Integer)
type Grid = [(Pos,Cell)]

create_cell :: Integer -> Integer -> Char -> (Pos, Cell)
create_cell x y v = ((x,y),C x y v False)

get_grid :: Integer -> Integer -> IO Grid
get_grid w h = get_grid' 0 w h
    where
        get_grid' :: Integer -> Integer -> Integer -> IO Grid
        get_grid' idx w h
            | idx == w = return []
            | otherwise = do
                line <- getLine
                others <- get_grid' (idx+1) w h
                return $ (map (create_cell idx h) line) ++ others

mark_border :: Grid -> Grid
mark_border = undefined

main :: IO ()
main = do
    str_params <- getLine
    let [w, h] = map read $ words str_params
    grid <- get_grid w h
    return ()

{-Idea: start from the border and mark every water accessible as ocean.-}
{-Then, look for every cell which is earth touching an ocean cell-}
