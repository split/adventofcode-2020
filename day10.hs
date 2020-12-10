import Data.List

part1 :: [Int] -> String
part1 adapters = "Part 1: " ++ show (count 1 * count 3)
  where
    count n = length $ filter (== n) diffs
    diffs = zipWith (-) (drop 1 adapters) adapters

countArranges :: [Int] -> [Int]
countArranges [] = []
countArranges (rating : restRatings) = count : nextCounts
  where
    nextCounts = countArranges restRatings
    connectable = takeWhile (<= rating + 3) restRatings
    count = max 1 $ sum $ take (length connectable) nextCounts

part2 :: [Int] -> String
part2 ratings = "Part 2: " ++ show (head $ countArranges ratings)

withOutlitAndBuiltIn :: Num a => [a] -> [a]
withOutlitAndBuiltIn ratings = 0 : ratings ++ [last ratings + 3]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . withOutlitAndBuiltIn . sort . map read . lines)