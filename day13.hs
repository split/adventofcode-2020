{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isDigit)
import Data.Foldable (maximumBy)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

progress :: Int -> Int -> Double
progress ts bus = fromIntegral ts / fromIntegral bus - fromIntegral (ts `div` bus)

timeToNext :: Int -> [Int] -> Int
timeToNext ts routes = ceiling (fromIntegral ts / fromIntegral nextLine) * nextLine - ts
  where
    nextLine = maximumBy (comparing (progress ts)) routes

findFreq :: (Int, Int) -> (Int, Int) -> (Int, Int)
findFreq (ts, iter) (delta, bus) =
  (,iter * bus) $ fromJust $ find (\n -> (n + delta) `rem` bus == 0) (iterate (+ iter) ts)

main :: IO ()
main = do
  ts <- read <$> getLine
  routes <- readRoutes . splitOn "," <$> getLine
  putStrLn $ "Part 1: " ++ show (timeToNext ts (map snd routes))
  putStrLn $ "Part 2: " ++ show (fst $ foldl findFreq (0, 1) routes)

readRoutes :: [String] -> [(Int, Int)]
readRoutes = map (second read) . filter (all isDigit . snd) . zip [0 ..]