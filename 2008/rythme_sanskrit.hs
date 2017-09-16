module Main where

import Control.Monad
import qualified Data.Map.Lazy as Map

type Mem = Map.Map Int Int

init_map :: Mem
init_map = Map.fromList [(1,1), (2,2), (3,3)]

fromJust :: Maybe a -> a
fromJust (Just a) = a

nb_sanskrit :: Int -> Int
nb_sanskrit n = fst $ nb_sanskrit' init_map n
    where
        nb_sanskrit' :: Mem -> Int -> (Int, Mem)
        nb_sanskrit' mem n =
            case Map.lookup n mem of
                Just v -> (v,mem)
                Nothing -> (res, Map.insert n res mem)
            where
                (v1, mem') = nb_sanskrit' mem (n-1)
                (v2, mem'') = nb_sanskrit' mem' (n-2)
                res = v1+v2

{-nb_sanskrit 1 = 1-}
{-nb_sanskrit 2 = 2-}
{-nb_sanskrit n = nb_sanskrit (n-2) + nb_sanskrit (n-1)-}

main :: IO ()
main = do
    n <- readLn
    print $ nb_sanskrit n
    return ()
