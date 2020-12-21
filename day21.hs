{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec

data Food = Food {ingredients :: S.Set String, allergens :: S.Set String}
  deriving (Show, Eq)

type AllergenMap = M.Map String (S.Set String)

ingredientsForAllergen :: [Food] -> AllergenMap
ingredientsForAllergen = M.fromListWith S.intersection . concatMap groupByAllergen
  where
    groupByAllergen food = (,ingredients food) <$> S.toList (allergens food)

safeIngredients :: [Food] -> S.Set String
safeIngredients foods = allIngredients foods S.\\ unsafe foods
  where
    allIngredients = S.unions . map ingredients
    unsafe = S.unions . M.elems . ingredientsForAllergen

solveAllergens :: AllergenMap -> AllergenMap
solveAllergens allergyMap
  | length solved == length allergyMap = allergyMap
  | otherwise = solveAllergens $ M.map (\a -> if isSolved a then a else a S.\\ solved) allergyMap
  where
    solved = S.unions $ M.elems $ M.filter isSolved allergyMap
    isSolved = (== 1) . length

part1 :: [Food] -> String
part1 foods = "Part 1: " ++ show (length $ filter (`S.member` safeIngredients foods) (concatMap (S.toList . ingredients) foods))

part2 :: [Food] -> String
part2 = ("Part 2: " ++) . intercalate "," . concatMap (S.toList . snd) . M.toAscList . solveAllergens . ingredientsForAllergen

main :: IO ()
main = interact (either show (unlines . sequence [part1, part2]) . parse (food `sepBy` char '\n') "")

food :: Parser Food
food = Food <$> ingredientsP <*> allergensP
  where
    ingredientsP = S.fromList <$> many1 letter `endBy` char ' '
    allergensP = S.fromList <$> (string "(contains " *> many1 letter `sepBy` string ", " <* char ')')
