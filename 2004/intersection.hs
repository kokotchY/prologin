module Main where

import Control.Monad

result :: (Integer, Integer) -> (Integer, Integer) -> Integer
result a@(s_a,e_a) b@(s_b,e_b)
    | snd first >= fst second = abs $ e-s
    | otherwise = 0
    where
        (first, second) =
            if s_a < s_b
                then (a,b)
                else (b,a)
        s = max (fst first) (fst second)
        e = min (snd first) (snd second)

test1 :: Integer
test1 = result ((-47686716),(-38491014)) ((-48528973),(-42411585))

test2 :: Integer
test2 = result ((-47698252),(-45630884)) ((-45440315),(-37180150))

test3 :: Integer
test3 = result ((-45440315),(-37180150)) ((-47698252),(-45630884))

main :: IO ()
main = do
    str_periode_a <- getLine
    str_periode_b <- getLine
    let [s_a, e_a] = map read $ words str_periode_a :: [Integer]
        [s_b, e_b] = map read $ words str_periode_b :: [Integer]
    print $ result (s_a,e_a) (s_b,e_b)
    return ()
