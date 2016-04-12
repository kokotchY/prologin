module Main where

import Control.Monad
import qualified Data.Map.Lazy as Map

type Mem = Map.Map Integer Integer

fib :: Integer -> Integer -> (Mem, Integer)
fib n m = fib' n m Map.empty
    where
        fib' :: Integer -> Integer -> Mem -> (Mem, Integer)
        fib' n m mem
            | n < m = (mem, 1)
            | otherwise =
                case Map.lookup n mem of
                    Just v -> (mem, v)
                    Nothing -> (result_mem, res)
                    where
                        (mem', v1) = fib' (n-1) m mem
                        (mem'', v2) = fib' (n-m) m mem'
                        res = v1+v2
                        result_mem = Map.insert n res mem''

force_lookup :: Integer -> Mem -> Integer
force_lookup n mem =
    case Map.lookup n mem of
        Just v -> v

main :: IO ()
main = do
    str_params <- getLine
    let [n,m] = map read $ words str_params
        solution = fib n m
    print $ snd solution
    return ()
