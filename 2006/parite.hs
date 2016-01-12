module Main where

import Control.Monad

get_cycle :: Int -> [Bool]
get_cycle n = cycle [even n, odd n]

is_property_valided :: [Int] -> Bool
is_property_valided list = all (\x -> fst x == snd x) $ zip (map even list) (get_cycle first)
    where
        first = head list

main :: IO ()
main = do
    _ <- getLine
    str_nb <- getLine
    let numbers = map read $ words str_nb :: [Int]
    if is_property_valided numbers
        then putStrLn "vrai"
        else putStrLn "faux"
    return ()
