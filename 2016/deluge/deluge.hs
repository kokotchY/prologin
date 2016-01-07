module Main where

main :: IO ()
main = do
    str_h <- getLine
    _ <- getLine
    str_montain_altitude <- getLine
    let h = read str_h :: Integer
    let altitude = map read $ words str_montain_altitude :: [Integer]
    if any (<h) altitude
        then putStrLn "1"
        else putStrLn "0"
