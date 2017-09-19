module Main where

import Control.Monad

index_code :: String -> [(Int, Char)] -> Maybe Int
index_code code [] = Nothing
index_code code line
    | length line < length code = Nothing
    | code == code' = Just $ fst $ head line
    | otherwise = index_code code (dropWhile (not . valid_char . snd) $ tail line)
    where
        code' = map snd $ take (length code) $ filter (valid_char . snd) line

valid_char :: Char -> Bool
valid_char = flip (elem) "ATGC"

main :: IO ()
main = do
    _ <- getLine
    code <- getLine
    _ <- getLine
    line <- liftM (zip [0..]) getLine
    let line' = dropWhile (not . valid_char . snd) line

    case index_code code line' of
        Just idx -> print idx
        Nothing -> putStrLn "-1"

    return ()
