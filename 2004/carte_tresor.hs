module Main where

import Control.Monad
import Data.List

follow :: Int -> Int -> (Int, Int) -> Char -> (Int, Int)
follow h w (pos_x, pos_y) m
    | m == 'O' && pos_x > 0 = (pos_x-1, pos_y)
    | m == 'E' && pos_x < w-1 = (pos_x+1, pos_y)
    | m == 'N' && pos_y > 0 = (pos_x, pos_y - 1)
    | m == 'S' && pos_y < h-1 = (pos_x, pos_y + 1)
    | otherwise = (pos_x, pos_y)


follow_plan :: Int -> Int -> String -> (Int, Int)
follow_plan h w = foldl' (follow h w) (0,0)

main :: IO ()
main = do
    _ <- getLine
    suite <- getLine
    str_pos <- getLine
    let [h, w] = map read $ words str_pos :: [Int]
        (res_x, res_y) = follow_plan h w suite
    putStrLn $ show res_y ++ " " ++ show res_x
    return ()
