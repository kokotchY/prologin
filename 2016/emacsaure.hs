module Main where

import Control.Monad

move :: (Int, Int) -> [Bool] -> Int ->  [String] -> Bool
move (x,y) (l:ls) lava_x grid
    | x == length (head grid) - 1 = True
    | can_move_right = move ((x+1), y) ls new_lava_x grid
    | can_move_bottom && x == lava_x = False
    | can_move_bottom && x /= lava_x = move (x, y+1) ls new_lava_x grid
    | otherwise = False
    where
        element_right = (grid !! y) !! (x+1)
        can_move_right = element_right == '.'
        can_move_bottom = y < length grid && element_bottom == '.'
        element_bottom = (grid !! (y+1) !! x)
        new_lava_x =
            if l
                then lava_x+1
                else lava_x

main :: IO ()
main = do
    str_params <- getLine
    let [m,n] = map read $ words str_params :: [Int]
    grid <- replicateM m getLine
    {-print $ move (0,0) grid-}
    if move (0,0) lava_cycle (-1) grid
        then putStrLn "1"
        else putStrLn "0"
    return ()
    where
        lava_cycle = cycle [False, True]
