module Main where

import Control.Monad
import Data.List

bit_parity :: [Int] -> Bool
bit_parity = even . length . fst . partition (==1)

main :: IO ()
main = do
    str_nb <- getLine
    let nb = map (read . (:"")) str_nb :: [Int]
    print $ if bit_parity nb
        then 1
        else 0
    return ()
