module Main where

import Control.Monad

occ :: Eq a => a -> [a] -> (a, Int)
occ a list = (a, length $ filter (==a) list)

result :: String -> Char
result word = fst $ get_max $ map (flip occ word) "TGCA"

get_max :: [(Char,Int)] -> (Char,Int)
get_max = foldr (\(a,nb) (c_max, max) -> if nb > max then (a,nb) else (c_max, max)) ('.', 0)

test1,test2,test3 :: Bool
test1 = result "ATTGCCATATCC" == 'C'
test2 = result "AAAACCCGGGTTT" == 'A'
test3 = result "BBAACT" == 'A'

allTests :: [Bool]
allTests = [test1,test2,test3]

main :: IO ()
main = do
    _ <- getLine
    word <- getLine
    putStrLn $ result word : []
    return ()
