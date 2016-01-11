module Main where

import Control.Monad

convert_grid :: [String] -> [[Bool]]
convert_grid = undefined

get_location :: Int -> Int -> [(Int, Int)]
get_location c_x c_y = diag2
{-vertical ++ horizontal ++ diag1 ++ diag2-}
    where
        vertical = [(x,c_y) | x <- [0..7]]
        horizontal = [(c_x, y) | y <- [0..7]]
        diag1 = [(x-c_x+1, x-c_y+1) | x <- [0..7], (x-c_x+1) >= 0 && (x-c_y+1) >= 0]
        diag2 = []

main :: IO ()
main = do
    grid <- replicateM 8 getLine
    return ()
