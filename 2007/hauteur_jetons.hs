module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params
        n = params !! 0
    grid <- replicateM n $ do
        line <- getLine
        let numbers = map read $ words line :: [Int]
        return $ numbers
    print $ maximum $ map (length . filter (==1)) $ transpose grid
    return ()
