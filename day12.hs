data Command = MoveNS Int | MoveEW Int | Steer Int | Forward Int
  deriving (Show)

command :: String -> Command
command input = case input of
  -- Action N means to move north by the given value.
  'N' : n -> MoveNS (read n)
  -- Action S means to move south by the given value.
  'S' : n -> MoveNS (- read n)
  -- Action E means to move east by the given value.
  'E' : n -> MoveEW (read n)
  -- Action W means to move west by the given value.
  'W' : n -> MoveEW (- read n)
  -- Action L means to turn left the given number of degrees.
  'L' : n -> Steer (- read n)
  -- Action R means to turn right the given number of degrees.
  'R' : n -> Steer (read n)
  -- Action F means to move forward by the given value in the direction the ship
  -- is currently facing.
  'F' : n -> Forward (read n)

data Ship = Ship {ns :: Int, ew :: Int, dir :: Int}
  deriving (Show)

posDeg :: Int -> Int
posDeg i = (360 + i) `rem` 360

runShip :: Ship -> Command -> Ship
runShip state (MoveNS n) = state {ns = ns state + n}
runShip state (MoveEW n) = state {ew = ew state + n}
runShip state (Steer n) = state {dir = posDeg (dir state + n)}
runShip state (Forward n) = runShip state (command $ dirToL (dir state) : show n)

dirToL :: (Eq a, Num a) => a -> Char
dirToL dir = case dir of
  0 -> 'E'
  90 -> 'S'
  180 -> 'W'
  270 -> 'N'

data Waypoint = Waypoint {wpNS :: Int, wpEW :: Int, ship :: Ship}

runWaypoint :: Waypoint -> Command -> Waypoint
runWaypoint state (MoveNS n) = state {wpNS = wpNS state + n}
runWaypoint state (MoveEW n) = state {wpEW = wpEW state + n}
runWaypoint state (Steer n) = rotate (posDeg n) state
runWaypoint state (Forward n) =
  state
    { ship =
        (ship state)
          { ns = ns (ship state) + wpNS state * n,
            ew = ew (ship state) + wpEW state * n
          }
    }

rotate :: Int -> Waypoint -> Waypoint
rotate deg state = case deg of
  90 -> state {wpNS = negate (wpEW state), wpEW = wpNS state}
  180 -> state {wpEW = negate (wpEW state), wpNS = negate (wpNS state)}
  270 -> state {wpNS = wpEW state, wpEW = negate (wpNS state)}
  _ -> state

manhattanDist :: Ship -> Int
manhattanDist s = abs (ns s) + abs (ew s)

initialShip = Ship {ns = 0, ew = 0, dir = 0}

part1 :: [Command] -> String
part1 = (++) "Part 1: " . show . manhattanDist <$> foldl runShip initialShip

part2 :: [Command] -> String
part2 = (++) "Part 2: " . show . manhattanDist . ship <$> foldl runWaypoint initialState
  where
    initialState = Waypoint {wpNS = 1, wpEW = 10, ship = initialShip}

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map command . lines)