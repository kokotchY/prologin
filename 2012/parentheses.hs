module Main where

import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map

data Parenthesis = P
    { getStart :: Int
    , getEnd :: Maybe Int
    }
    deriving Show

type List = Map.Map Int Parenthesis

get_word_in_parentheseis :: String -> Parenthesis -> String
get_word_in_parentheseis sentence (P start (Just end)) = word
    where
        sentence' = drop start sentence
        sentence'' = take end sentence'
        word = filter (not . (`elem` ['(',')'])) sentence''

get_parenthesis :: String -> List
get_parenthesis sentence = get_parenthesis' (zip [0..] sentence) 1 Map.empty
get_parenthesis' :: [(Int, Char)] -> Int -> List -> List
get_parenthesis' [] _ p = p
get_parenthesis' ((pos, c):xs) p_pos p
    | c == '(' = get_parenthesis' xs (p_pos+1) $ Map.insert p_pos (new_p pos) p
    | c == ')' = get_parenthesis' xs p_pos $ (update_highest_p pos p)
    | otherwise = get_parenthesis' xs p_pos p

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

get_word_at_pos :: String -> Int -> String
get_word_at_pos sentence pos = get_word_in_parentheseis sentence p
    where
        p =
            case Map.lookup pos list of
                Just v -> v
                Nothing -> error "Impossible"
        list = get_parenthesis sentence

update_highest_p :: Int ->  List -> List
update_highest_p pos list = Map.adjust (\p -> p { getEnd = Just pos }) pos_highest list
    where
        (pos_highest, parenthesis) = get_highest_p list

get_highest_p :: List -> (Int, Parenthesis)
get_highest_p list = maximumBy (\p1 p2 -> compare (getStart $ snd p1) (getStart $ snd p2)) $ filter (not . isJust . getEnd . snd) $ Map.toAscList list

new_p :: Int -> Parenthesis
new_p start = P start Nothing

test_sentence :: String
test_sentence = "((CAN)E)T(O(N))"

get_nth_word :: Int -> String -> String
get_nth_word = get_nth_word' 0 0 0
    where
        get_nth_word' :: Int -> Int -> Int -> Int -> String -> String
        get_nth_word' p_level w_p_level w_nb w (x:xs)
            | x == '(' && w_nb < w = get_nth_word' (p_level+1) w_p_level (w_nb+1) w xs
            | x == ')' && w_nb < w = get_nth_word' (p_level-1) w_p_level w_nb w xs
            | x == '(' && w_nb == w = get_nth_word' p_level p_level w_nb w xs
            | w_nb == w && x /= ')' = x:get_nth_word' p_level w_p_level w_nb w xs
            | w_nb == w && x == '(' = get_nth_word' (p_level+1) w_p_level w_nb w xs
            | w_nb == w && x == ')' && w_p_level /= p_level = get_nth_word' (p_level-1) w_p_level w_nb w xs
            | w_nb == w && x == ')' && w_p_level == p_level = []

main :: IO ()
main = do
    _ <- getLine
    sentence <- getLine
    str_p <- getLine
    let p = read str_p :: Int
    putStrLn $ get_word_at_pos sentence p
    return ()
