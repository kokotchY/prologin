module Main where

import Control.Monad
import Data.List

something :: String -> [String]
something = last . takeWhile ((<=80) . length . intercalate " ") . inits . words

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    return ()
