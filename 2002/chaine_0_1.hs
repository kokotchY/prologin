module Main where

import Control.Monad

convert :: String -> [Int]
convert [] = []
convert (x:xs) = (read (x:[])) : convert xs

convert2 :: [(Int, Int)] -> [Int]
convert2 = map snd

split :: (a -> Bool) -> [a] -> ([a], [a])
split f [] = ([],[])
split f (x:xs)
    | f x = (x:a, b)
    | otherwise = (a, x:b)
    where
        (a, b) = split f xs

to_count :: Int -> (Int, Int) -> Bool
to_count idx (pos, value)
    | pos < idx && value == 1 = True
    | pos >= idx && value == 0 = True
    | otherwise = False

calc :: Int -> [Int] -> Int
calc index = undefined

list :: [Int]
list = [0,0,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,0,1,1,0,0,1,1,1,0,1,0,0,1,1,0,1,0,1,0,1,0,1,0,1,0]

list2 :: [(Int, Int)]
list2 = zip [0..] list

main :: IO ()
main = do
    _ <- getLine
    str_list <- getLine
    let list = convert str_list
    print list
    return ()
