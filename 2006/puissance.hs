module Main where

import Control.Monad

puissance :: Int -> Int -> Int
puissance nb power = product $ take power $ repeat nb

main :: IO ()
main = do
    str_nb <- getLine
    str_power <- getLine
    let nb = read str_nb
        power = read str_power
    print $ puissance nb power
    return ()
