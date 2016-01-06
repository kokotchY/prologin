module Main where

occ :: Int -> [Int] -> Int
occ nb list = length $ filter (==nb) list

main :: IO ()
main = do
    _ <- getLine
    str_nb <- getLine
    let list = map read $ words str_nb :: [Int]
        list_occ = map (\x -> (x, occ x list)) list
        result = filter ((==1) . snd) list_occ
    print $ fst $ result !! 0
