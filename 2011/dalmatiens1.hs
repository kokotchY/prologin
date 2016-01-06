module Main where

import Control.Monad

invert :: Int -> Int
invert 0 = 1
invert 1 = 0

neg_image :: [[Int]] -> [[Int]]
neg_image = map (map invert)

main :: IO ()
main = do
    str_n <- getLine
    _ <- getLine
    let n = read str_n
    image <- replicateM n $ do
        line <- getLine
        return $ map read $ words line
    let neg_img = neg_image image
    mapM_ (putStrLn . (concatMap show)) neg_img
    return ()
