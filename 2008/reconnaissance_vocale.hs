module Main where

import Control.Monad
import Data.List

type Nodes = [Node]

data Node = Node
    { nodeFrom :: Integer
    , nodeTo :: Integer
    , nodeChar :: Char
    , nodeScore :: Integer
    }
    deriving Show

convert :: String -> Node
convert line = Node i j c s
    where
        w = words line
        i = read $ w !! 0
        j = read $ w !! 1
        c = head $ w !! 2
        s = read $ w !! 3

getNodesForCharFrom :: Char -> Integer -> Nodes -> Nodes
getNodesForCharFrom c from = filter (\n -> nodeFrom n == from && nodeChar n == c)

getMaxProba :: Integer -> Nodes -> String -> (Integer, Integer)
getMaxProba from _ [] = (from, 1)
getMaxProba from nodes (w:ws) = proba
    where
        nodesForChar = getNodesForCharFrom w from nodes
        nodesProba = map (bla nodes ws) nodesForChar
        proba =
            if length nodesForChar > 0
                then maximumBy (\x y -> compare (snd x) (snd y)) nodesProba
                else (0, -1)

bla :: Nodes -> String -> Node -> (Integer, Integer)
bla nodes ws n = (fst max_proba, snd max_proba * nodeScore n)
    where
        max_proba = getMaxProba (nodeFrom n) nodes ws

test_graph :: [Node]
test_graph = [Node {nodeFrom = 2, nodeTo = 1, nodeChar = 'a', nodeScore = 5},Node {nodeFrom = 2, nodeTo = 0, nodeChar = 'b', nodeScore = 1},Node {nodeFrom = 1, nodeTo = 0, nodeChar = 'a', nodeScore = 1},Node {nodeFrom = 1, nodeTo = 2, nodeChar = 'a', nodeScore = 4},Node {nodeFrom = 2, nodeTo = 2, nodeChar = 'a', nodeScore = 3}]

main :: IO ()
main = do
    str_n <- getLine
    str_init <- getLine
    str_m <- getLine
    let n = read str_n :: Integer
        m = read str_m
        init = read str_init :: Integer
    graph <- replicateM m $ do
        line <- getLine
        return $ convert line
    word <- getLine
    let (dest, proba) = getMaxProba init graph word
    print $ if proba > 0
        then dest
        else (-1)
    return ()
