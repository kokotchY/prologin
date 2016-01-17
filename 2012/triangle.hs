module Main where

import Control.Monad

triangle :: Int -> [String]
triangle size = map (triangle_line size) [1..size]

triangle_line :: Int -> Int -> String
triangle_line size n = space ++ ['/'] ++ in_space ++ ['\\']
    where
        space = replicate (size-n) ' '
        in_space = replicate ((n-1)*2) in_char
        in_char = if n == size
            then '_'
            else ' '

main :: IO ()
main = do
    str_n <- getLine
    let n = read str_n :: Int
    mapM_ putStrLn $ triangle n
    return ()



{-123/\-}
{-12/12\-}
{-1/1234\-}
{-/123456\-}

{-1234/\-}
{-123/12\-}
{-12/1234\-}
{-1/123456\-}
{-/12345678\-}

{-12345/\-}
{-1234/12\-}
{-123/1234\-}
{-12/123456\-}
{-1/12345678\-}
{-/1234567890\-}
