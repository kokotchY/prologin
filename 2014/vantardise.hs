
main = do
    _ <- getLine
    str_sizes <- getLine
    let sizes = map read $ words str_sizes :: [Int]
    putStrLn $ show $ foldr1 max sizes
