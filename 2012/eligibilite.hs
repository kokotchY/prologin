module Main where

is_valid :: Int -> Int -> Int -> Bool
is_valid d m y
    | y == 1991 && d >= 1 && m >= 5 = True
    | y > 1991 = True
    | otherwise = False

main :: IO ()
main = do
    str_day <- getLine
    str_month <- getLine
    str_year <- getLine
    let day = read str_day
        month = read str_month
        year = read str_year
    if is_valid day month year
        then putStrLn "1"
        else putStrLn "0"

