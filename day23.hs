import Data.Char (digitToInt)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

data Move = Move
  { cups :: [Int],
    pickedUp :: [Int],
    current :: Int,
    destination :: Int
  }
  deriving (Show)

pickDestination :: Int -> [Int] -> Int
pickDestination current = (!! 1) . dropWhile (/= current) . cycle . reverse . sort . (current :)

move :: [Int] -> [Int]
move cups =
  trace
    ("cups: (" ++ show current ++ ") " ++ show [pickedUp, cupsLeft] ++ "\npick up: " ++ show pickedUp ++ "\ndestination: " ++ show destination)
    (insertAfter destination pickedUp cupsLeft ++ [current])
  where
    current = head cups
    destination = pickDestination current cupsLeft
    pickedUp = take 3 $ drop 1 cups
    cupsLeft = filter (\c -> c /= current && c `notElem` pickedUp) cups

insertAfter :: Eq a => a -> [a] -> [a] -> [a]
insertAfter item items target = fromMaybe target replaced
  where
    replaced = (\(a, b) -> a ++ (item : items) ++ b) . flip splitAt target <$> elemIndex item target

solve :: [Int] -> String
solve = show

main :: IO ()
main = interact (solve . map digitToInt)

example :: [Int]
example = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Int]
input = [3, 2, 6, 5, 1, 9, 4, 7, 8]