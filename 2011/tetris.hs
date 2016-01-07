module Main where

import Control.Monad

main :: IO ()
main = do
    str_n <- getLine
    _ <- getLine
    let n = read str_n
    grid <- replicateM n $ do
        line <- getLine
        return $ all (=="1") $ words line
    putStrLn $ show $ length $ filter (==True) grid
    return ()
