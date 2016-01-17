module Main where

import Control.Monad
import Data.List (isPrefixOf)

get_code_position :: String -> String -> Int
get_code_position = get_code_position' 0
    where
        get_code_position' :: Int -> String -> String -> Int
        get_code_position' _ [] _ = -1
        get_code_position' pos genome@(g:gs) code
            | not (valid g) = get_code_position' (pos+1) gs code
            | valid g && code `isPrefixOf` (filter valid genome) = pos
            | otherwise = get_code_position' (pos+1) gs code
            where
                valid = (`elem` "ATGC")

main :: IO ()
main = do
    _ <- getLine
    code <- getLine
    _ <- getLine
    genome <- getLine
    print $ get_code_position genome code
    return ()
