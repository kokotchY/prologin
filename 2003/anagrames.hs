module Main where

import Control.Monad
import Data.List

my_sort :: (String, String) -> (String, String)
my_sort (a,b) = (new_a, new_b)
    where
        new_a = min a b
        new_b = max a b

get_anagrames :: String -> [(String, String)] -> [(String, String)]
get_anagrames w str = map my_sort $ map ((,) w) $ filter (/= w) $ map fst $ filter ((==w') . snd) str
    where
        w' = sort w

get_words :: String -> [(String, String)]
get_words = map (\w -> (w, sort w)) . words

get_nb_couples :: [(String, String)] -> Int
get_nb_couples w = length $ nub $ filter ((/=0) . length) $ map (\x -> get_anagrames (fst x) w) w

main :: IO ()
main = do
    _ <- getLine
    str <- getLine
    let sentence = get_words str
    print $ get_nb_couples sentence
    return ()
