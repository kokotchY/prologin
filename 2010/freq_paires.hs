module Main where

import Control.Monad
import Data.Char
import qualified Data.Map.Lazy as Map
import Data.List

type Store = Map.Map String Integer

incr_index :: Store -> String -> Store
incr_index store key =
    case Map.lookup key store of
        Nothing -> Map.insert key 1 store
        Just v -> Map.insert key (v+1) store

calc_occur :: [String] -> Store
calc_occur = foldl' incr_index Map.empty

gen_list :: String -> [String]
gen_list [x] = []
gen_list (x:xs@(y:ys)) = [x:y:[]] ++ gen_list xs

get_most_occ :: String -> Integer
get_most_occ = snd . maximumBy (\c1 c2 -> compare (snd c1) (snd c2)) . Map.toList . calc_occur . filter only_letters . gen_list

only_letters :: String -> Bool
only_letters = all (`elem` ['a'..'z'])

main :: IO ()
main = do
    _ <- getLine
    str_text <- getLine
    let text = map toLower str_text
        result = get_most_occ text
    print result
    return ()
