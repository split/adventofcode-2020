import Control.Monad
import Data.Maybe (isJust, mapMaybe)

type Coord = (Int, Int)

type Coordinated a = (Coord, a)

dirs :: [Coord]
dirs = [(x, y) | x <- [(-1) .. 1], y <- [(-1) .. 1], (x, y) /= (0, 0)]

sumCoord :: Coord -> Coord -> Coord
sumCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

coordinated :: [String] -> [Coordinated Char]
coordinated lines = [((x, y), c) | (y, ys) <- zip [0 ..] lines, (x, c) <- zip [0 ..] ys]

adjacent :: [Coordinated Char] -> Coord -> String
adjacent plane seat = mapMaybe ((`lookup` plane) . sumCoord seat) dirs

seeable :: [Coordinated Char] -> Coord -> String
seeable plane seat = mapMaybe see dirs
  where
    see dir = msum $ filter (/= Just '.') $ takeWhile isJust $ map (`lookup` plane) (pointsInDir dir)
    pointsInDir dir = iterate (sumCoord dir) (sumCoord dir seat)

createRule :: Int -> String -> Char -> Char
createRule limit adj seat
  | seat == 'L' && '#' `notElem` adj = '#'
  | seat == '#' && length (filter (== '#') adj) >= limit = 'L'
  | otherwise = seat

gameOfPlane :: ([Coordinated Char] -> Coordinated Char -> Coordinated Char) -> [Coordinated Char] -> Int
gameOfPlane rule plane = occupied !! length notRepeating
  where
    notRepeating = takeWhile (/= True) $ zipWith (==) occupied (drop 1 occupied)
    occupied = countOccupied <$> iterate (\p -> map (rule p) p) plane
    countOccupied = length . filter ((== '#') . snd)

part1 :: [Coordinated Char] -> String
part1 = (++) "Part 1: " . show . gameOfPlane rule
  where
    rule plane (p, s) = (p, createRule 4 (adjacent plane p) s)

part2 :: [Coordinated Char] -> String
part2 = (++) "Part 2: " . show . gameOfPlane rule
  where
    rule plane (p, s) = (p, createRule 5 (seeable plane p) s)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . coordinated . lines)
