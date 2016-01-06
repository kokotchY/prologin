module Main where

import Control.Monad
import Data.List

pos_hole :: String -> [Int]
pos_hole = pos_hole' 0
    where
        pos_hole' :: Int -> String -> [Int]
        pos_hole' _ [] = []
        pos_hole' pos (x:xs)
            | x == 'o' = pos:pos_hole' (pos+1) xs
            | otherwise = pos_hole' (pos+1) xs

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params :: [Int]
        n = params !! 0
        m = params !! 1
    list <- replicateM n $ do
        line <- getLine
        return $ pos_hole line

    if (all ((==1) . length) $ concatMap (\x -> map (intersect x) list) list)
        then putStrLn "1"
        else putStrLn "0"
