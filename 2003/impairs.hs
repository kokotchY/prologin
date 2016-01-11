module Main where

import Control.Monad
import Data.List

main :: IO ()
main = do
    str_nb <- getLine
    let [nb1, nb2] = map read $ words str_nb :: [Int]
        odd_numbers = filter odd $ [nb1..nb2]
        result = concat $ intersperse " " $ map show odd_numbers
    putStrLn result
    return ()
