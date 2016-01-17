module Main where

import Control.Monad

list = [(2,5), (3,8), (5,14), (2,6), (4,13), (6,17), (3,10), (1,4)]

main :: IO ()
main = do
    str_n <- getLine
    str_m <- getLine
    let n = read str_n
    list <- replicateM n $ do
        line <- getLine
        let [t, p] = map (fromIntegral . read) $ words line :: [Double]
        return $ (t,p)
    return ()
