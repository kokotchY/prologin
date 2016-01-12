module Main where

import Control.Monad
import Data.List

trim_road :: String -> [String]
trim_road road = road'
    where
        remove_empty = dropWhile (=='0')
        road' = group $ remove_empty $ reverse $ remove_empty road

is_safe :: String -> Bool
is_safe = all ((>=15) . length) . filter (all (=='0')) . trim_road

main :: IO ()
main = do
    _ <- getLine
    road <- getLine
    if is_safe road
        then print 1
        else print 0
    return ()
