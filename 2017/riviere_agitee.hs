module Main where

import Control.Monad

type Pos = (Int,Int)

coast_locations :: [(Pos,Pos)] -> [Pos]
coast_locations list = filter (not . (`elem` src)) dst
    where
        (src, dst) = unzip list

reach_start :: [(Pos,Pos)] -> Pos -> Bool
reach_start rivers pos
    | pos == (0,0) = True
    | otherwise = any (reach_start rivers) src
    where
        src = map fst $ filter ((==) pos . snd) rivers

data1, data2, data3 :: [(Pos, Pos)]
data1 = [((0,0),(1,1)), ((1,1),(0,0))]
data2 = [((1,5),(3,6)),((0,0),(2,2)),((2,2),(1,5))]
data3 = [((0,0),(1,1)),((1,1),(1,2)),((1,2),(2,2)),((2,2),(2,1)),((2,1),(1,1)),((2,2),(3,3))]

main :: IO ()
main = do
    n <- readLn
    stream <- replicateM n $ do
        [x1,y1,x2,y2] <- liftM (map read . words) getLine
        return ((x1,y1),(x2,y2)) :: IO (Pos,Pos)
    let loc = coast_locations stream
        solutions = filter (reach_start stream) loc
        first_solution = head solutions
    if length loc > 0 && length solutions > 0
        then putStrLn $ show (fst first_solution) ++ " " ++ show (snd first_solution)
        else putStrLn "JAMAIS"
    return ()
