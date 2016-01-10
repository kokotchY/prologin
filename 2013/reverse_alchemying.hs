module Main where

import Control.Monad
import Data.List

data Ingredient = Ingredient Int String deriving Show

data Recipe = Recipe Ingredient [Ingredient] deriving Show

get_ingredient :: Int -> [Ingredient] -> Ingredient
get_ingredient nb [] = error $ "nope for " ++ show nb
get_ingredient nb (i@(Ingredient id name):is)
    | nb == id = i
    | otherwise = get_ingredient nb is

create_recipe :: [Ingredient] -> String -> Recipe
create_recipe ingredients recipe = Recipe name all_ingredients
    where
        recipe_part = map read $ words recipe :: [Int]
        name = get_ingredient (head recipe_part) ingredients
        all_ingredients = map (\x -> get_ingredient x ingredients) $ tail recipe_part

get_recipe :: Ingredient -> [Recipe] -> Recipe
get_recipe i@(Ingredient nb name) [] = Recipe i []
get_recipe i@(Ingredient nb name) (r:rs)
    | get_name r == name = r
    | otherwise = get_recipe i rs
    where
        get_name :: Recipe -> String
        get_name (Recipe (Ingredient _ r_name) _) = r_name

get_list_recipe :: Recipe -> [Recipe] -> [Ingredient] -> [String]
get_list_recipe (Recipe (Ingredient id name) r_ingredients) recipes ing = list
    where
        previous_element = concatMap get_r_from_i r_ingredients
        list = if length r_ingredients == 0
            then previous_element
            else previous_element ++ [name ++ " = " ++ ingredients_string]
        ingredients_string = concat $ intersperse " + " (map get_i_name r_ingredients)
        get_i_name :: Ingredient -> String
        get_i_name (Ingredient _ name) = name
        get_r_from_i :: Ingredient -> [String]
        get_r_from_i i = get_list_recipe (get_recipe i recipes) recipes ing

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
    mapM_ putStrLn $ get_list_recipe (head recipes) recipes ingredients
    return ()
