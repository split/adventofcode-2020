import Data.List
import qualified Data.Set as Set

data Coord = SpaceCoord Int Int Int | HyperCoord Int Int Int Int
  deriving (Eq, Show, Ord)

type Space = Set.Set Coord

isActive :: Coord -> Space -> Bool
isActive = Set.member

activeNeighbors :: Coord -> Space -> [Coord]
activeNeighbors coord space = filter (`isActive` space) (neighborCoords coord)

getNewNeighbors :: [Coord] -> Space -> Space
getNewNeighbors inactive space = Set.fromAscList $ filter rule inactive
  where
    rule nc = length (activeNeighbors nc space) == 3

processCube :: Coord -> Space -> Space
processCube coord space
  | length active == 2 || length active == 3 = coord `Set.insert` newNeighbors
  | otherwise = newNeighbors
  where
    (active, inactive) = partition (`isActive` space) (neighborCoords coord)
    newNeighbors = getNewNeighbors inactive space

simulate :: Space -> Space
simulate space = Set.unions updatedCubeSpaces
  where
    updatedCubeSpaces = Set.map (`processCube` space) space

main :: IO ()
main = interact (unlines . sequence simulations . lines)
  where
    runSimulation = show . length . last . take 7 . iterate simulate
    simulations =
      [ ("Part 1: " ++) . runSimulation . readCoords (\x y -> SpaceCoord x y 0),
        ("Part 2: " ++) . runSimulation . readCoords (\x y -> HyperCoord x y 0 0)
      ]

readCoords :: (Int -> Int -> Coord) -> [String] -> Space
readCoords f input = Set.fromList [f x y | (y, ys) <- zip [0 ..] input, (x, state) <- zip [0 ..] ys, state == '#']

neighborCoords :: Coord -> [Coord]
neighborCoords (SpaceCoord dx dy dz) = [SpaceCoord (dx + x) (dy + y) (dz + z) | x <- [(-1) .. 1], y <- [(-1) .. 1], z <- [(-1) .. 1], (x, y, z) /= (0, 0, 0)]
neighborCoords (HyperCoord dx dy dz dw) = [HyperCoord (dx + x) (dy + y) (dz + z) (dw + w) | x <- [(-1) .. 1], y <- [(-1) .. 1], z <- [(-1) .. 1], w <- [(-1) .. 1], (x, y, z, w) /= (0, 0, 0, 0)]