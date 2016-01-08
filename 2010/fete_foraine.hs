module Main where

import Control.Monad

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1,y1) (x2,y2)
    | x1 == x2 = abs $ y1 - y2
    | y1 == y2 = abs $ x1 - x2

parse_str_coord :: String -> (Int, Int)
parse_str_coord = parse_coord . map read . words

parse_coord :: [Int] -> (Int, Int)
parse_coord (x:y:xs) = (x,y)

main :: IO ()
main = do
    str_g <- getLine
    str_nb <- getLine
    let g = parse_str_coord str_g
        nb = read str_nb
    attractions <- replicateM nb $ do
        line <- getLine
        return $ parse_str_coord line
    let tot = sum $ map ((*2) . dist g) attractions
    print tot
    return ()
