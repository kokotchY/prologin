module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n
    matrix <- replicateM n $ do
        line <- getLine
        return $ words line
    let solution = transpose matrix
    mapM_ putStrLn $ map unwords solution
    return ()
