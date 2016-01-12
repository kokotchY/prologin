module Main where

import Control.Monad

gen_palette :: String -> [(Char, (Int, Int))]
gen_palette palette = zip palette $ map (\x -> ((x*interval), (x+1)*interval-1)) [0..size-1]
    where
        size = length palette
        interval = div 256 size

get_pixel_value :: Int -> [(Char, (Int, Int))] -> Char
get_pixel_value value palette = c
    where
        c = fst $ head $ filter (\(ch, (min_v, max_v)) -> (value >= min_v) && (value <= max_v)) palette

color :: [[Int]] -> [(Char, (Int, Int))] -> [[Char]]
color img palette = map (map (\p -> get_pixel_value p palette)) img

img :: [[Int]]
img = [ [1,1,1,1,1], [1,128,128,128,1], [1,128,128,128,1], [1,128,128,128,1], [1,1,1,1,1] ]

main :: IO ()
main = do
    str_size <- getLine
    let [w, h] = map read $ words str_size :: [Int]
    drawing <- replicateM h $ do
        line <- getLine
        let numbers = map read $ words line :: [Int]
        return $ numbers
    _ <- getLine
    str_palette <- getLine
    let palette = gen_palette str_palette
        result = color drawing palette
    mapM_ putStrLn result
    return ()
