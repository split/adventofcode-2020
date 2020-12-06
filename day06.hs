import Data.List
import Data.List.Split

part1 :: [String] -> String
part1 groups = "Part 1: " ++ show (sum $ map (length . nub . filter (/= '\n')) groups)

part2 :: [String] -> String
part2 groups = "Part 2: " ++ show (sum $ map (length . foldl1 intersect . lines) groups)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . splitOn "\n\n")