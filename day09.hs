import Data.List
import Relude.Extra.Tuple (toSnd)

type Weakness = Int

findContiguousSet :: [Int] -> Weakness -> Maybe [Int]
findContiguousSet xmas weakness = findContiguousSet' past
  where
    past = takeWhile (/= weakness) xmas
    findContiguousSet' = find ((== weakness) . sum) . concatMap tails . inits . filter (< weakness)

findWeakness :: Int -> [Int] -> Maybe Weakness
findWeakness preambleSize input = last <$> find isWeakness (preambles input)
  where
    isWeakness chunk = last chunk `notElem` pairSums (take preambleSize chunk)
    preambles = map (take (preambleSize + 1)) . tails
    pairSums = map sum . filter ((== 2) . length) . subsequences

main :: IO ()
main = interact (unlines . sequence [part1 . snd, part2] . toSnd (findWeakness 25) . map read . lines)

part1 :: Maybe Weakness -> String
part1 = (++) "Part 1: " <$> maybe "All valid" show

part2 :: ([Int], Maybe Weakness) -> String
part2 (xmas, weakness) = "Part 2: " ++ maybe "No result" (show . sumOfBoundaries) contiguousSet
  where
    contiguousSet = weakness >>= findContiguousSet xmas
    sumOfBoundaries set = minimum set + maximum set