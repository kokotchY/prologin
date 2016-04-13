module Main where

import Control.Monad
import qualified Data.Map.Strict as Map

type Pos = (Integer, Integer)
type Grid = Map.Map Pos Integer

bla1 :: [[Integer]] -> [(Integer, [(Integer, Integer)])]
bla1 = zip [0..] . map (zip [0..])

bla2 :: (Integer, [(Integer, Integer)]) -> _
bla2 list = map (\(y,l) -> bla3 y l) list

bla3 :: Integer -> [(Integer, Integer)] -> [((Integer, Integer), Integer)]
bla3 y l = map (\(x,v) -> ((x,y),v)) l

parse_grid :: [[Integer]] -> Grid
parse_grid = undefined

grid :: [[Integer]]
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
