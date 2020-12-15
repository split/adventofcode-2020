import Data.List.Split (splitOn)
import qualified Data.Map as Map

type GameHistory = Map.Map Int Int

getTurn :: Int -> [(Int, Int)] -> Int
getTurn targetTurn initial = runGame targetTurn (Map.fromList $ init initial) (last initial)

runGame :: Int -> GameHistory -> (Int, Int) -> Int
runGame targetTurn hist lastNum@(lastValue, lastTurn) =
  if turn == targetTurn then answer else runGame targetTurn nextHist (answer, turn)
  where
    answer = getAnswer lastNum hist
    nextHist = Map.insert lastValue lastTurn hist
    turn = lastTurn + 1

getAnswer :: (Int, Int) -> GameHistory -> Int
getAnswer (lastNum, lastTurn) hist = maybe 0 (lastTurn -) (lastNum `Map.lookup` hist)

main :: IO ()
main = interact (unlines . solve . flip zip [1 ..] . map read . splitOn ",")
  where
    solve =
      sequence
        [ ("Part 1: " ++) . show . getTurn 2020,
          ("Part 2: " ++) . show . getTurn 30000000
        ]