module Main where

import Control.Monad

convert_arr :: Int -> [Int]
convert_arr x
    | x < 10 = [x]
    | otherwise = convert_arr start ++ [remain]
    where
        (start, remain) = x `divMod` 10

gen :: [Int] -> Int
gen = foldl (\x y -> x*10+y) 0

is_kapreka :: Int -> Bool
is_kapreka n = any (\(a,b) -> gen a + gen b == n) $ map (flip splitAt arr) [1..length arr]
    where
        arr = convert_arr (n^2)

main :: IO ()
main = do
    n <- readLn :: IO Int
    if is_kapreka n
        then putStrLn "1"
        else putStrLn "0"
    return ()
