module Main where

move :: Int -> String -> [Int] -> Int
move pos [] pipes = fixPos pos pipes
move pos (x:xs) pipes
    | x == 'A' = move (fixPos (pos+1) pipes) xs pipes
    | x == 'R' = move (fixPos (pos-1) pipes) xs pipes
    | x == 'T' = move (fixPos (getPosThroughPipe pipes pos) pipes) xs pipes
    | otherwise = error $ "Impossible " ++ [x]
    where
        getPosThroughPipe :: [Int] -> Int -> Int
        getPosThroughPipe pipes pos =
            if newPos == 0
                then length pipes
                else newPos
            where newPos = pos + (pipes !! (pos+1)) - 1


fixPos :: Int -> [Int] -> Int
fixPos newPos pipes = (newPos + length pipes) `mod` length pipes

main :: IO ()
main = do
    _ <- getLine
    str_values <- getLine
    _ <- getLine
    moves <- getLine
    return ()

doTest = move 1 "AART" [1,2,1,3,4,1]
doTest2 = move 1 "ATRR" [1,0,2,1]
