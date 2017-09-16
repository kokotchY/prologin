module Main where

import Control.Monad
import Data.List

is_word_grid :: [String] -> String -> Bool
is_word_grid grid w = or [
    any (\l -> w `isSubsequenceOf` l) grid,
    any (\l -> w' `isSubsequenceOf` l) grid,
    any (\l -> w `isSubsequenceOf` l) grid',
    any (\l -> w' `isSubsequenceOf` l) grid'
    ]
    where
        w' = reverse w
        grid' = transpose grid

main :: IO ()
main = do
    [n,m,p] <- liftM (map read . words) getLine :: IO [Int]
    words_list <- replicateM p getLine
    grid <- replicateM n getLine
    let found_words = filter (is_word_grid grid) words_list
    print $ length found_words
    return ()
