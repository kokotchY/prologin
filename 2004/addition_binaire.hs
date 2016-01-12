module Main where

import Control.Monad

add_bin :: [Int] -> [Int] -> [Int]
add_bin a b = reverse $ add_bin' 0 (reverse a) (reverse b)
    where
        add_bin' :: Int -> [Int] -> [Int] -> [Int]
        add_bin' _ [] [] = []
        add_bin' carry (x:xs) (y:ys) = add_bin' c xs ys ++ [value]
            where
                (value, c) = add x y carry

add :: Int -> Int -> Int -> (Int, Int)
add x y carry
    | x+y+carry < 2 = (x+y+carry, 0)
    | x+y+carry == 2 = (0, 1)
    | x+y+carry == 3 = (1, 1)
main :: IO ()
main = do
    _ <- getLine
    str_l <- getLine
    let l = read str_l
    numbers <- replicateM l $ do
        line <- getLine
        let nb = map (read . (:"")) line :: [Int]
        return nb
    print numbers
    return ()
