module Main where

import Control.Monad
import Data.List

convert :: String -> [String]
convert [] = []
convert list = aa : convert remain
    where
        (aa, remain) = splitAt 3 list

main :: IO ()
main = do
    str_n <- getLine
    str_m <- getLine
    let n = read str_n :: Int
        m = read str_m :: Int
    dict <- replicateM m $ do
        line <- getLine
        let parts = words line
        return (head parts, head $ tail parts)
    sequence <- getLine
    let res = mconcat $ intersperse (Just " ") $ map (flip lookup dict) $ convert sequence
    putStrLn $ case res of
        Just name -> name
        Nothing -> "Unknown"
    return ()
