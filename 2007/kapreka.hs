
module Main where

import Control.Monad

is_kapreka :: Int -> Bool
is_kapreka nb = 
    where
        square = nb*nb

main :: IO ()
main = do
    str_nb <- getLine
    let nb = read str_nb
    if is_kapreka nb
        then print 1
        else print 0
    return ()
