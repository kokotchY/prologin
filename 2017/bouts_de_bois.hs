module Main where

import Control.Monad
import Data.List

prepare_sizes :: [Int] -> [(Int, Int)]
prepare_sizes = zip [0..] . reverse . sort

get_biggest_couple :: [(Int,Int)] -> Maybe ((Int,Int),(Int,Int))
get_biggest_couple [] = Nothing
get_biggest_couple [x] = Nothing
get_biggest_couple (x:y:xs)
    | snd x == snd y = Just (x,y)
    | snd x - 1 == snd y = Just (x,y)
    | otherwise = get_biggest_couple (y:xs)

type Couple = (Int,Int)
type Couple' = (Couple, Couple)

remove_couple :: (Couple, Couple) -> [Couple] -> [Couple]
remove_couple (c1, c2) = filter (\c -> fst c /= fst c1 && fst c /= fst c2)

get_2_couple :: [Couple] -> Maybe (Couple, [Couple])
get_2_couple list = do
    c1 <- get_biggest_couple list
    let list' = remove_couple c1 list
    c2 <- get_biggest_couple list'
    let list'' = remove_couple c2 list'
    let (s1,s2) = (snd $ snd c1, snd $ snd c2)
    return ((s1,s2), list'')

get_couples :: [Couple] -> Maybe [Couple]
get_couples list =
    case get_2_couple list of
        Just (c, remain) ->
            case get_couples remain of
                Nothing -> return [c]
                Just r -> return (c:r)
        Nothing -> Nothing


calc_tot_area :: [Couple] -> Int
calc_tot_area = sum . map calc_area

calc_area :: Couple -> Int
calc_area (a,b) = a*b


main :: IO ()
main = do
    [n, k] <- liftM (map read . words) getLine :: IO [Int]
    sizes <- liftM (map read . words) getLine :: IO [Int]
    return ()
