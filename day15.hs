{-# LANGUAGE LambdaCase #-}

import Control.Monad.Reader
import Control.Monad.ST (ST, runST)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed.Mutable as VecM

type GameHistory s = VecM.STVector s Int

getTurn :: Int -> [(Int, Int)] -> Int
getTurn targetTurn initial = runST $ do
  hist <- VecM.replicate targetTurn (-1)
  forM_ (init initial) (uncurry $ VecM.write hist)
  runGame hist targetTurn (last initial)

runGame :: GameHistory s -> Int -> (Int, Int) -> ST s Int
runGame hist targetTurn (lastValue, lastTurn) = do
  answer <-
    VecM.read hist lastValue >>= \case
      (-1) -> return 0
      a -> return (lastTurn - a)

  VecM.write hist lastValue lastTurn
  if lastTurn + 1 == targetTurn
    then return answer
    else runGame hist targetTurn (answer, lastTurn + 1)

main :: IO ()
main = interact (unlines . solve . flip zip [1 ..] . map read . splitOn ",")
  where
    solve =
      sequence
        [ ("Part 1: " ++) . show . getTurn 2020,
          ("Part 2: " ++) . show . getTurn 30000000
        ]