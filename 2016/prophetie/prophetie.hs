module Main where

import Control.Monad

calc :: Int -> [Int] -> Int
calc x = sum . map (\y -> abs (x-y)) . filter (<= x)

range :: [Int] -> [Int]
range list = [minimum list..maximum list]

resultat :: Int -> [Int] -> Int
resultat x alts = head $ dropWhile ((<x) . flip calc alts) $ range alts

main :: IO ()
main = do
    str_v <- getLine
    _ <- getLine
    str_alt <- getLine
    let v = read str_v :: Int
        altitudes = map read $ words str_alt :: [Int]
    print $ resultat v altitudes
    return ()
