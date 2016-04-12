module Main where

import Control.Monad

data Pyramide = Empty | Node Integer Pyramide Pyramide

parse_pyramide :: [Integer] -> Pyramide
parse_pyramide = undefined

find_max_pyramide :: Pyramide -> Integer
find_max_pyramide Empty = 0
find_max_pyramide (Node value left right) = value + max (find_max_pyramide left) (find_max_pyramide right)


main :: IO ()
main = do
    return ()

