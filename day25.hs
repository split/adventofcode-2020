module Main where

transformSubject :: Int -> Int -> Int
transformSubject sub = go 1
  where
    go value 0 = value
    go value i = go (step sub value) $! i - 1

-- 
loops :: Int -> Int -> Int
loops sub key = length $ takeWhile (/= key) $ iterate (step sub) 1

{-# INLINE step #-}
step :: Int -> Int -> Int
step sub value = value * sub `rem` 20201227


main :: IO ()
main = print (transformSubject doorpub (loops 7 keypub))

keypub, doorpub :: Int
keypub = 12092626
doorpub = 4707356