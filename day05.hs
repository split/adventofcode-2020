import Data.Bifunctor
import Data.List

reduceSeat :: Char -> (Int, Int) -> [Char] -> (Int, Int)
reduceSeat pickUpper =
  foldl
    ( \range rule ->
        if rule == pickUpper
          then first (half ceiling . (+ snd range)) range
          else second (half floor . (+ fst range)) range
    )
  where
    half f x = f (fromIntegral x / 2)

calcRid :: String -> Int
calcRid s = row * 8 + col
  where
    row = fst $ reduceSeat 'B' (0, 127) (take 7 s)
    col = snd (reduceSeat 'R' (0, 7) (drop 7 s))

part1 :: [String] -> String
part1 = (++) "Part 1: " <$> show . maximum . map calcRid

part2 :: [String] -> String
part2 lines =
  "Part 2: "
    ++ maybe "no seat" show ((+ 1) . fst <$> find (\(a, b) -> abs (a - b) == 2) seatPairs)
  where
    ids = sort $ map calcRid lines
    seatPairs = zip ids (drop 1 ids)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . lines)
