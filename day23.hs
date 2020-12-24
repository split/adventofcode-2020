{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.ST
import Data.Array.Base (thawSTUArray)
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char (intToDigit, digitToInt)


type Cup = Int
type CupCircle = UArray Cup Cup
type STCupCircle s = STUArray s Cup Cup

buildCupCircle :: [Cup] -> CupCircle
buildCupCircle cups = array (minimum cups, maximum cups) $ zip cups (tail $ cycle cups)

playCups :: Int -> [Cup] -> CupCircle
playCups rounds initialCups = runSTUArray $ do
  cups <- thawSTUArray (buildCupCircle initialCups)
  cups <$ (iterate (>>= playRound cups) (pure (head initialCups)) !! rounds)
  where
    playRound :: STCupCircle s -> Int -> ST s Int
    playRound cups' current = do
      bounds <- getBounds cups'
      pickedUp <- readPickedUps
      next <- readArray cups' (last pickedUp)
      writeArray cups' current next

      let destination = pickDestination pickedUp bounds (current - 1)
      readArray cups' destination >>= writeArray cups' (last pickedUp)
      writeArray cups' destination (head pickedUp)

      return next
        where 
          readPickedUps = do
            pickUp1 <- readArray cups' current
            pickUp2 <- readArray cups' pickUp1
            pickUp3 <- readArray cups' pickUp2
            pure [pickUp1, pickUp2, pickUp3]

pickDestination :: [Cup] -> (Cup, Cup) -> Cup -> Cup
pickDestination pickedUp (minCup, maxCup) = go
  where
    go cup
      | cup < minCup = go maxCup
      | cup `notElem` pickedUp = cup
      | otherwise = go (cup - 1)

oneMillionCups cups = cups ++ [maximum cups + 1 .. 1000000]

part1 = ("Part 1: " ++) . pickCups . playCups 100
  where
    pickCups cups = map intToDigit . takeWhile (/= 1) $ iterate (cups !) (cups ! 1)  

part2 = ("Part 2: " ++) . show . pickStars . playCups 10000000 . oneMillionCups
  where
    pickStars cups = (cups ! 1) * (cups ! (cups ! 1))

solve = show

main :: IO ()
main = interact (solve . map digitToInt)

example :: [Int]
example = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Int]
input = [3, 2, 6, 5, 1, 9, 4, 7, 8]