module Main where

import Data.List (find, sort)

-- >>> calcSid "BFFFBBFRRR"
-- 567
-- >>> calcSid "FFFBBBFRRR"
-- 119
-- >>> calcSid "BBFFBBFRLL"
-- 820
calcSid :: String -> Int
calcSid = foldl (\sid p -> sid * 2 + bit p) 0
  where
    bit 'B' = 1
    bit 'R' = 1
    bit _ = 0

part1 :: [String] -> String
part1 = (++) "Part 1: " <$> show . maximum . map calcSid

part2 :: [String] -> String
part2 lines =
  "Part 2: "
    ++ maybe "no seat" show ((+ 1) . fst <$> find (\(a, b) -> b - a == 2) seatPairs)
  where
    sids = sort $ map calcSid lines
    seatPairs = zip sids (drop 1 sids)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . lines)
