module Main where

import Data.List

printCreneaux :: Int -> IO ()
printCreneaux nb = do
    let top = " _  "
        middle = "| |_"
    putStrLn $ (init $ concat $ take nb $ repeat top)
    putStrLn $ (init $ concat $ take nb $ repeat middle)

printWall :: Int -> Int -> IO ()
printWall w h
    | h == 0 = return ()
    | otherwise = do
        let line = "|" ++ (concat $ take w $ repeat " ") ++ "|"
            castle = concat $ intersperse "\n" (take h $ repeat line)
        putStrLn castle

printBottom :: Int -> IO ()
printBottom w = putStrLn $ "|" ++ (concat $ take w $ repeat "_") ++ "|"

main :: IO ()
main = do
    str_n <- getLine
    str_h <- getLine
    let n = read str_n :: Int
        h = read str_h :: Int
    displayCastle n h

displayCastle :: Int -> Int -> IO ()
displayCastle n h = do
    printCreneaux n
    printWall (4*n-3) (h-2)
    printBottom (4*n-3)
