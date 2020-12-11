import Control.Monad
import Data.List (drop, filter, map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import Prelude hiding (drop, filter, lookup, map)

type Coord = (Int, Int)

type Plane = Map.Map Coord Char

dirs :: [Coord]
dirs = [(x, y) | x <- [(-1) .. 1], y <- [(-1) .. 1], (x, y) /= (0, 0)]

sumCoord :: Coord -> Coord -> Coord
sumCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

coordinated :: [String] -> Plane
coordinated rows = Map.fromList [((x, y), c) | (y, ys) <- zip [0 ..] rows, (x, c) <- zip [0 ..] ys]

adjacent :: Plane -> Coord -> String
adjacent plane seat = mapMaybe ((`Map.lookup` plane) . sumCoord seat) dirs

seeable :: Plane -> Coord -> String
seeable plane seat = mapMaybe see dirs
  where
    see = msum . filter (/= Just '.') . takeWhile isJust . map (`Map.lookup` plane) . pointsInDir
    pointsInDir dir = iterate (sumCoord dir) (sumCoord dir seat)

seatStateRule :: Int -> String -> Char -> Char
seatStateRule limit cared state
  | state == 'L' && '#' `notElem` cared = '#'
  | state == '#' && length (filter (== '#') cared) >= limit = 'L'
  | otherwise = state

gameOfPlane :: (Plane -> Coord -> Char -> Char) -> Plane -> Int
gameOfPlane rule plane = occupied !! length notRepeating
  where
    notRepeating = takeWhile (/= True) $ zipWith (==) occupied (drop 1 occupied)
    occupied = countOccupied <$> iterate (\plane' -> Map.mapWithKey (rule plane') plane') plane
    countOccupied = length . Map.filter (== '#')

part1 :: Plane -> String
part1 = (++) "Part 1: " . show . gameOfPlane rule
  where
    rule plane seat state = seatStateRule 4 (adjacent plane seat) state

part2 :: Plane -> String
part2 = (++) "Part 2: " . show . gameOfPlane rule
  where
    rule plane seat state = seatStateRule 5 (seeable plane seat) state

main :: IO ()
main = interact (unlines . sequence [part1, part2] . coordinated . lines)
