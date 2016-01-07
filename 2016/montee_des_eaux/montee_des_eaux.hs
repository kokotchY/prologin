module Main where

main :: IO ()
main = do
    str_h <- getLine
    _ <- getLine
    str_alt <- getLine
    let h = read str_h :: Integer
        alt = map read $ words str_alt :: [Integer]
    putStrLn $ show $ sum $ map (\x -> if x < h then abs (x-h) else 0) alt
