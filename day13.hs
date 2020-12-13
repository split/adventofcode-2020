{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isDigit)
import Data.Foldable (minimumBy)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

timeToNext :: Int -> [Int] -> Int
timeToNext ts = uncurry (*) <$> minimumBy (comparing snd) . map next
  where
    next bus = (bus, ((ts `div` bus + 1) * bus) - ts)

findFreq :: [(Int, Int)] -> Int
findFreq = fst . foldl syncFreq (0, 1)
  where
    syncFreq (ts, iter) (delta, bus) =
      (,iter * bus) $ fromJust $ find (\n -> (n + delta) `rem` bus == 0) (iterate (+ iter) ts)

main :: IO ()
main = do
  ts <- read <$> getLine
  routes <- readRoutes <$> getLine
  putStrLn $ "Part 1: " ++ show (timeToNext ts (map snd routes))
  putStrLn $ "Part 2: " ++ show (findFreq routes)

readRoutes :: String -> [(Int, Int)]
readRoutes = map (second read) . filter (all isDigit . snd) . zip [0 ..] . splitOn ","