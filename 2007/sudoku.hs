module Main where

import Control.Monad

type Sudoku = [[Int]]

checkLines :: Sudoku -> Bool
checkLines = checkLines' 0
    where
        checkLines' :: Int -> Sudoku -> Bool
        checkLines' 9 _ = True
        checkLines' n sudoku = (all (==1) $ map (\x -> occ x (sudoku !! n)) [1..9]) && checkLines' (n+1) sudoku

is_valid_sudoku :: Sudoku -> Bool
is_valid_sudoku sudoku = checkLines sudoku

occ :: Eq a => a -> [a] -> Int
occ v = length . filter (==v)

main :: IO ()
main = do
    sudoku <- replicateM 9 $ do
        line <- getLine
        let numbers = map read  $ words line :: [Int]
        return numbers
    print sudoku
    if is_valid_sudoku sudoku
        then print 1
        else print 0
    return ()
