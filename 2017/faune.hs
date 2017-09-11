module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    n <- readLn
    animals <- replicateM n (getLine >> getLine)
    print $ length $ nub animals
    return ()
