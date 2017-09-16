module Main where

import Control.Monad
import qualified Data.Map.Lazy as Map

type Pos = (Int,Int)
type Grid = Map.Map Pos Int

data1,data2 :: Grid
data1 = Map.fromList [((0,0),3),((0,1),2),((0,2),5),((0,3),6),((1,0),4),((1,1),5),((1,2),4),((1,3),2),((2,0),5),((2,1),1),((2,2),9),((2,3),5),((3,0),6),((3,1),2),((3,2),4),((3,3),3)]
data2 = Map.fromList [((0,0),1),((0,1),2),((0,2),3),((0,3),1),((0,4),5),((0,5),2),((0,6),7),((1,0),2),((1,1),1),((1,2),3),((1,3),5),((1,4),6),((1,5),8),((1,6),5),((2,0),1),((2,1),4),((2,2),5),((2,3),6),((2,4),7),((2,5),3),((2,6),3),((3,0),4),((3,1),5),((3,2),6),((3,3),7),((3,4),5),((3,5),2),((3,6),6),((4,0),5),((4,1),6),((4,2),7),((4,3),8),((4,4),4),((4,5),6),((4,6),7),((5,0),6),((5,1),8),((5,2),1),((5,3),4),((5,4),9),((5,5),8),((5,6),2),((6,0),2),((6,1),5),((6,2),8),((6,3),3),((6,4),5),((6,5),6),((6,6),9)]

value :: Grid -> Pos -> Int
value grid value = fromJust $ Map.lookup value grid

fromJust :: Maybe a -> a
fromJust (Just a) = a

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

around_pos :: Pos -> [Pos]
around_pos (x,y) =
    [ (x+1, y)
    , (x-1, y)
    , (x, y+1)
    , (x, y-1)
    ]

max_value :: Grid -> Pos -> Int -> Int
max_value grid pos 0 = value grid pos
max_value grid pos k = value grid pos + maximum (map (\p -> max_value grid p (k-1)) (filter (\p -> isJust (Map.lookup p grid)) (around_pos pos)))

readGrid :: Int -> Int -> IO Grid
readGrid x y = do
    list <- replicateM y $ liftM (map read . words) getLine :: IO [[Int]]
    let list' = concatMap (\(y,line) -> map (\(x,c) -> ((x,y),c)) $ zip [0..] line) $ zip [0..] list
        res = foldl (\m (p,c) -> Map.insert p c m) Map.empty list'
    return res

main :: IO ()
main = do
    [x,y,n] <- liftM (map read . words) getLine
    grid <- readGrid x y
    print $ max_value grid (0,0) (n-1)
    return ()
