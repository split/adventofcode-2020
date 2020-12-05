import Data.Bifunctor
import Data.List

reducePartitions :: Char -> (Int, Int) -> [Char] -> Int
reducePartitions pickUpper initialRegion partitions =
  fst $ foldl cropRegion initialRegion partitions
  where
    half x = fromIntegral x / 2
    cropRegion region partition =
      if partition == pickUpper
        then first (ceiling . half . (+ snd region)) region
        else second (floor . half . (+ fst region)) region

calcSid :: String -> Int
calcSid s = row * 8 + col
  where
    row = reducePartitions 'B' (0, 127) (take 7 s)
    col = reducePartitions 'R' (0, 7) (drop 7 s)

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
