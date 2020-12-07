{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.String.Utils (replace)

type Bag = String

type BagMap a = [(Bag, a)]

type BagRules = [(Int, Bag)]

part1 :: BagMap BagRules -> String
part1 bagmap = show . length $ filter (containsBag bagsInside "shiny gold bag") bags
  where
    bags = map fst bagmap
    bagsInside = map snd . getRules bagmap

containsBag :: (Bag -> [Bag]) -> Bag -> Bag -> Bool
containsBag bagsInside seekedBag bag =
  seekedBag `elem` bags || any (containsBag bagsInside seekedBag) bags
  where
    bags = bagsInside bag

part2 :: BagMap BagRules -> String
part2 bagmap = "Part 2: " ++ show (countBags bagmap "shiny gold bag")

countBags :: BagMap BagRules -> Bag -> Int
countBags bags bag = sum $ map (\(count, rule) -> count + count * countBags bags rule) rules
  where
    rules = getRules bags bag

getRules :: BagMap BagRules -> Bag -> BagRules
getRules bags bag = fromMaybe [] (lookup bag bags)

main :: IO ()
main = do
  interact (unlines . sequence [part1, part2] . parseBags . lines . cleanup)

parseBags :: [String] -> BagMap BagRules
parseBags = map (second parseRules . cutOn " contain ")
  where
    parseRules :: String -> BagRules
    parseRules "no other bag" = []
    parseRules rules = map (first read . cutOn " ") (splitOn ", " rules)

cleanup :: String -> String
cleanup s = replace "bags" "bag" (replace "." "" s)

cutOn :: String -> String -> (String, String)
cutOn cutter = (\(a : b) -> (a, intercalate cutter b)) . splitOn cutter