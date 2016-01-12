module Main where

import Control.Monad

result :: Int -> Int -> Int -> Int -> Int
result s_a e_a s_b e_b
    | e_a >= s_b = abs $ e-s
    | otherwise = 0
    where
        s = max s_a s_b
        e = min e_a e_b

test1 :: Int
test1 = result (-47686716) (-38491014) (-48528973) (-42411585)

test2 :: Int
test2 = result (-47698252) (-45630884) (-45440315) (-37180150)

main :: IO ()
main = do
    str_periode_a <- getLine
    str_periode_b <- getLine
    let [s_a, e_a] = map read $ words str_periode_a :: [Int]
        [s_b, e_b] = map read $ words str_periode_b :: [Int]
    print $ result s_a e_a s_b e_b
    return ()
