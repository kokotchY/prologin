module Main where

import Control.Monad
import Data.List

data Ingredient = Ingredient Int String deriving Show

data Recipe = Recipe Ingredient [Ingredient] deriving Show

get_recipe :: Int -> [Ingredient] -> Ingredient
get_recipe nb [] = error $ "nope for " ++ show nb
get_recipe nb (i@(Ingredient id name):is)
    | nb == id = i
    | otherwise = get_recipe nb is

create_recipe :: [Ingredient] -> String -> Recipe
create_recipe ingredients recipe = Recipe name all_ingredients
    where
        recipe_part = map read $ words recipe :: [Int]
        name = get_recipe (head recipe_part) ingredients
        all_ingredients = map (\x -> get_recipe x ingredients) $ tail recipe_part

get_list_recipe :: Recipe -> [Ingredient] -> [String]
get_list_recipe (Recipe (Ingredient id name) r_ingredients) ing = previous_element ++ [name ++ " = " ++ ingredients_string]
    where
        previous_element = ["haha"]
        ingredients_string = concat $ intersperse " + " (map get_i_name r_ingredients)
        get_i_name :: Ingredient -> String
        get_i_name (Ingredient _ name) = name

main :: IO ()
main = do
    str_params <- getLine
    let params = map read $ words str_params :: [Int]
        n = params !! 0
        m = params !! 1
    rec <- replicateM m getLine
    ing <- replicateM n getLine
    let ingredients = zipWith Ingredient [0..] ing
        recipes = map (create_recipe ingredients) rec
    mapM_ putStrLn $ get_list_recipe (head recipes) ingredients
    return ()
