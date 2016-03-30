module Main where

import Control.Monad
import Data.List

data Cell = C
    { getX :: Integer
    , getY :: Integer
    , value :: Int
    }
    deriving Show

type Pos = (Integer, Integer)
type Grid = [(Pos, Cell)]

convert :: String -> [Int]
convert [] = []
convert (x:xs) = (read (x:[])) : convert xs

get_grid :: IO Grid
get_grid = get_grid' 0
    where
        get_grid' :: Integer -> IO Grid
        get_grid' 6 = return []
        get_grid' y = do
            line <- getLine
            let nb = map (\(x,v) -> ((x,y), C x y v)) $ zip [0..] $ convert line
            others <- get_grid' (y+1)
            return $ nb ++ others

get_cells :: [Pos] -> Grid -> [Cell]
get_cells positions grid = map fromJust $ filter isJust $ map (get_cell grid) positions

get_cell :: Grid -> Pos -> Maybe Cell
get_cell grid pos = lookup pos grid

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

fromJust :: Maybe a -> a
fromJust (Just a) = a

get_winner :: Grid -> Int
get_winner grid = get_winner' grid grid
    where
        get_winner' :: Grid -> Grid -> Int
        get_winner' [] _ = 0
        get_winner' (g:gs) grid
            | length results > 0 = head (head results)
            | otherwise = get_winner' gs grid
            where
                results = filter f_not_0 $ filter f_size_4 $ concatMap (\f -> group $ map value (cells f)) [h,v,d1,d2]
                f_size_4 = (==4).length
                f_not_0 = (\x -> head x /= 0)
                cells f = get_cells (f (fst g)) grid

test_grid :: Grid
test_grid = [((0,0),C {getX = 0, getY = 0, value = 0}),((1,0),C {getX = 1, getY = 0, value = 0}),((2,0),C {getX = 2, getY = 0, value = 1}),((3,0),C {getX = 3, getY = 0, value = 0}),((4,0),C {getX = 4, getY = 0, value = 0}),((5,0),C {getX = 5, getY = 0, value = 0}),((6,0),C {getX = 6, getY = 0, value = 0}),((0,1),C {getX = 0, getY = 1, value = 0}),((1,1),C {getX = 1, getY = 1, value = 0}),((2,1),C {getX = 2, getY = 1, value = 2}),((3,1),C {getX = 3, getY = 1, value = 2}),((4,1),C {getX = 4, getY = 1, value = 0}),((5,1),C {getX = 5, getY = 1, value = 0}),((6,1),C {getX = 6, getY = 1, value = 0}),((0,2),C {getX = 0, getY = 2, value = 0}),((1,2),C {getX = 1, getY = 2, value = 1}),((2,2),C {getX = 2, getY = 2, value = 2}),((3,2),C {getX = 3, getY = 2, value = 1}),((4,2),C {getX = 4, getY = 2, value = 0}),((5,2),C {getX = 5, getY = 2, value = 0}),((6,2),C {getX = 6, getY = 2, value = 0}),((0,3),C {getX = 0, getY = 3, value = 0}),((1,3),C {getX = 1, getY = 3, value = 2}),((2,3),C {getX = 2, getY = 3, value = 2}),((3,3),C {getX = 3, getY = 3, value = 1}),((4,3),C {getX = 4, getY = 3, value = 0}),((5,3),C {getX = 5, getY = 3, value = 0}),((6,3),C {getX = 6, getY = 3, value = 0}),((0,4),C {getX = 0, getY = 4, value = 2}),((1,4),C {getX = 1, getY = 4, value = 2}),((2,4),C {getX = 2, getY = 4, value = 1}),((3,4),C {getX = 3, getY = 4, value = 2}),((4,4),C {getX = 4, getY = 4, value = 1}),((5,4),C {getX = 5, getY = 4, value = 0}),((6,4),C {getX = 6, getY = 4, value = 0}),((0,5),C {getX = 0, getY = 5, value = 1}),((1,5),C {getX = 1, getY = 5, value = 2}),((2,5),C {getX = 2, getY = 5, value = 1}),((3,5),C {getX = 3, getY = 5, value = 1}),((4,5),C {getX = 4, getY = 5, value = 2}),((5,5),C {getX = 5, getY = 5, value = 1}),((6,5),C {getX = 6, getY = 5, value = 0})]

h,v,d1,d2 :: Pos ->  [Pos]
h (x,y) = map (\v -> (x+v,y)) [-3..3]
v (x,y) = map (\v -> (x,y+v)) [-3..3]
d1 (x,y) = map (\v -> (x+v, y-v)) [-3..3]
d2 (x,y) = map (\v -> (x+v, y+v)) [-3..3]

main :: IO ()
main = do
    grid <- get_grid
    print $ get_winner grid
    return ()
