module Main where
import Data.List

diff :: Integer -> [Integer] -> [Integer]
diff prev [] = []
diff prev (x:xs) = (x-prev):diff x xs

main :: IO ()
main = do
    _ <- getLine
    str_temps <- getLine
    let temps = map read $ words str_temps :: [Integer]
        result = first : diff first end
        first = head temps
        end = tail temps
    putStrLn $ unwords $ map show result
