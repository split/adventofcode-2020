import Data.List
import Data.List.Split (splitOn)

part1 :: [String] -> String
part1 = (++) "Part 1: " . show . length . concatMap (nub . filter (/= '\n'))

part2 :: [String] -> String
part2 = (++) "Part 2: " . show . length . concatMap (foldl1 intersect . lines)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . splitOn "\n\n")