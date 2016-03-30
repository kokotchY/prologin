module Main where

import Control.Monad

type Pos = (Int,Int)

get_piece_position :: [String] -> [String] -> [Pos]
get_piece_position piece puzzle = []

get_piece_at :: [String] -> Pos -> [String]
get_piece_at puzzle (x,y) = puzzle''
    where
        puzzle' = take 4 $ drop y puzzle
        puzzle'' = map (take 4 . drop x) puzzle'

can_place_piece :: [String] -> [String] -> Bool
can_place_piece piece puzzle = can_place_piece' (concat piece) (concat puzzle)
    where
        can_place_piece' :: String -> String -> Bool
        can_place_piece' [] [] = True
        can_place_piece' (x:xs) (y:ys)
            | x == '1' && y == '1' = False
            | otherwise = can_place_piece' xs ys

test_piece :: [String]
test_piece = ["0110","0110","1111","0011"]

test_puzzle :: [String]
test_puzzle = ["1111111111","1110111111","1100111001","1100111001","1000011101","1100011111","1111011111","1100100111","1110110011","1110111111"]

piece_placable :: [String] -> [String] -> Bool
piece_placable piece puzzle = any (\pos -> can_place_piece piece (get_piece_at puzzle pos)) positions
    where
        positions = [(x,y) | x <- [0..6], y <- [0..6]]

main :: IO ()
main = do
    piece <- replicateM 4 getLine
    puzzle <- replicateM 10 getLine
    if piece_placable piece puzzle
        then putStrLn "1"
        else putStrLn "0"
    return ()
