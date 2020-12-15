{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import qualified Data.Map as Map

type GameHistory = Map.Map Int Int

getTurn :: Int -> [(Int, Int)] -> Int
getTurn targetTurn initial = runGame targetTurn (Map.fromList $ init initial) (last initial)

runGame :: Int -> GameHistory -> (Int, Int) -> Int
runGame targetTurn hist lastNum =
  if turn == targetTurn then answer else runGame targetTurn nextHist (answer, turn)
  where
    (answer, nextHist) = getAnswer lastNum hist
    turn = snd lastNum + 1

getAnswer :: (Int, Int) -> GameHistory -> (Int, GameHistory)
getAnswer (lastNum, lastTurn) hist = answer searchResult
  where
    searchResult = maybe 0 (lastTurn -) (lastNum `Map.lookup` hist)
    answer = (,Map.insert lastNum lastTurn hist)

main :: IO ()
main = interact (unlines . solve . flip zip [1 ..] . map read . splitOn ",")
  where
    solve =
      sequence
        [ ("Part 1: " ++) . show . getTurn 2020,
          ("Part 2: " ++) . show . getTurn 30000000
        ]