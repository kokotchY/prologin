module Main where

import Control.Monad

parse_real :: String -> Float
parse_real str = read nb
    where
        (ent, dec) = get_parts str
        nb = ent ++ "." ++ dec

get_parts :: String -> (String, String)
get_parts str
    | '.' `elem` str = (ent, tail $ snd parts)
    | otherwise = (str, "0")
    where
        parts = span (/= '.') str
        ent = if fst parts /= "" then fst parts else "0"

comp :: Float -> Float -> Int
comp nb1 nb2
    | nb1 > nb2 = 1
    | nb1 < nb2 = -1
    | nb1 == nb2 = 0

comp_s :: String -> String -> Int
comp_s s1 s2 = comp (parse_real s1) (parse_real s2)

test1, test2, test3, test4, test5 :: Bool
test6, test7, test8, test9, test10 :: Bool
test1 = get_parts "123.456" == ("123", "456")
test2 = get_parts "414" == ("414", "0")
test3 = get_parts ".512" == ("0", "512")
test4 = get_parts "0000042.42" == ("0000042", "42")
test5 = get_parts "1.0410" == ("1", "0410")
test6 = parse_real "123.456" == 123.456
test7 = parse_real "414" == 414.0
test8 = parse_real ".512" == 0.512
test9 = parse_real "0000042.42" == 42.42
test10 = parse_real "1.0410" == 1.0410

allTests :: Bool
allTests = and [test1,test2,test3,test4,test5,test6, test7, test8, test9, test10]

test_s1, test_s2, test_s3 :: Bool
test_s1 = comp_s "42.69" "69.42" == -1
test_s2 = comp_s "1024" "00414" == 1
test_s3 = comp_s "0" ".00" == 0

allTests_s :: Bool
allTests_s = and [test_s1, test_s2, test_s3]

main :: IO ()
main = do
    _ <- getLine
    str_nb1 <- getLine
    _ <- getLine
    str_nb2 <- getLine
    let nb1 = parse_real str_nb1
        nb2 = parse_real str_nb2
    print $ comp nb1 nb2
    return ()
