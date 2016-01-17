{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.List

occ :: Eq a => a -> [a] -> Int
occ a = length . filter (==a)

get_occ_letters :: String -> [(Char, Int)]
get_occ_letters list = map (\x -> (x, occ x list)) $ nub list

is_possible :: String -> String -> Bool
is_possible message test_message = is_possible' test_msg_letters message_letters
    where
        !message_letters = get_occ_letters message
        !test_msg_letters = get_occ_letters test_message
        is_possible' :: [(Char,Int)] -> [(Char,Int)] -> Bool
        is_possible' [] _ = True
        is_possible' ((c, nb):xs) list = case lookup c list of
            Nothing -> False
            Just nb2 -> nb2 >= nb && is_possible' xs list

main :: IO ()
main = do
    _ <- getLine
    message <- getLine
    _ <- getLine
    test_message <- getLine
    if is_possible message test_message
        then putStrLn "1"
        else putStrLn "0"
    return ()
