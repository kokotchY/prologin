module Main where

import Control.Monad

rotation :: String -> Int -> String
rotation sentence nb = take size $ drop (size - nb) cycle_sentence
    where
        size = length sentence
        cycle_sentence = cycle sentence

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    str_rot <- getLine
    let rot = read str_rot
        result = rotation sentence rot
    putStrLn result
    return ()
