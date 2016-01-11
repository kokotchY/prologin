module Main where

import Control.Monad

parse_seconds :: Int -> (Int, Int, Int)
parse_seconds seconds = (h, m, s)
    where
        h = seconds `div` 3600
        m = (seconds - (h*3600)) `div` 60
        s = (seconds - (h*3600) - (m*60))

humanTime :: (Int, Int, Int) -> String
humanTime (h, m, s) = hour ++ ":" ++ minute ++ ":" ++ second
    where
        hour = pad_time h
        minute = pad_time m
        second = pad_time s
        pad_time :: Int -> String
        pad_time v = if v <= 9 then "0" ++ show v else show v

main :: IO ()
main = do
    str_seconds <- getLine
    let seconds = read str_seconds
        (h, m, s) = parse_seconds seconds
    putStrLn $ humanTime (h,m,s)
    return ()
