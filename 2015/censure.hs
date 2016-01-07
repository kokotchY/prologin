module Main where

censure :: (String, Bool) -> String
censure (word,True) = concat $ take (length word) $ repeat "*"
censure (word,False) = word


gen_pattern :: Int -> [Int] -> [Bool]
gen_pattern n = gen_pattern' [0..n]
    where
        gen_pattern' :: [Int] -> [Int] -> [Bool]
        gen_pattern' list [] = take (length list) $ repeat False
        gen_pattern' (x:xs) (y:ys)
            | x == y = True:gen_pattern' xs ys
            | otherwise = False:gen_pattern' xs (y:ys)

main :: IO ()
main = do
    _ <- getLine
    str_sentence <- getLine
    str_m <- getLine
    str_pos_censure <- getLine
    let sentence = words str_sentence
        m = read str_m :: Int
        pos_censure = map ((\x -> x-1) . read) $ words str_pos_censure :: [Int]
    print pos_censure
    putStrLn $ unwords $ map censure $ zip sentence (gen_pattern (length sentence) pos_censure)
