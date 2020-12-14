{-# LANGUAGE TupleSections #-}

import Data.Bits
import Data.Char (digitToInt, intToDigit)
import Data.List.Utils (replace)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace
import Numeric (showIntAtBase)
import Text.ParserCombinators.Parsec

newtype Mask = Mask String deriving (Show)

data ProgramInst = MemInst Integer Integer | MaskInst Mask
  deriving (Show)

type Memory = Map.Map Integer Integer

floatingMasks :: Mask -> [Integer]
floatingMasks (Mask mask) = map readBits (maskCombs (replace "1" "0" mask))

maskCombs :: String -> [String]
maskCombs [] = []
maskCombs ('X' : rest) = concatMap (maskCombs . (: rest)) ['1', '0']
maskCombs [c] = [[c]]
maskCombs (c : rest) = map (c :) $ maskCombs rest

readBits :: String -> Integer
readBits = foldl (\acc v -> acc * 2 + fromIntegral (digitToInt v)) 0

bitmaskFor :: [(Char, Char)] -> String -> Integer
bitmaskFor bmap = readBits . map (\c -> fromMaybe c (Map.lookup c (Map.fromList bmap)))

applyMask :: Mask -> Integer -> Integer
applyMask mask value = (appendMask mask .|. value) .&. filterMask mask

appendMask :: Mask -> Integer
appendMask (Mask mask) = bitmaskFor [('X', '0')] mask

filterMask :: Mask -> Integer
filterMask (Mask mask) = bitmaskFor [('X', '1')] mask

data PState = PState {mask :: Mask, memory :: Memory}

initialState :: PState
initialState = PState {mask = Mask (replicate 36 '1'), memory = Map.empty}

runLegacy :: [ProgramInst] -> Integer
runLegacy = sum . memory . foldl runInst initialState
  where
    runInst state (MaskInst newMask) = state {mask = newMask}
    runInst state (MemInst addr value) =
      state {memory = Map.insert addr (applyMask (mask state) value) (memory state)}

runProgram :: [ProgramInst] -> Integer
runProgram = sum . memory . foldl runInst initialState
  where
    runInst state (MaskInst newMask) = state {mask = newMask}
    runInst state (MemInst addr value) = state {memory = Map.union newValues (memory state)}
      where
        newValues = Map.fromList $ map (,value) addresses
        addresses = map (.|. (addr .&. appendMask (mask state))) (floatingMasks (mask state))

main :: IO ()
main = interact (unlines . sequence [("Part 1: " ++) . show . runLegacy, ("Part 2: " ++) . show . runProgram] . parseProgram)

parseMem :: Parser ProgramInst
parseMem = do
  try (string "mem[")
  address <- many digit
  string "] = "
  value <- many digit
  return $ MemInst (read address) (read value)

parseMask :: Parser ProgramInst
parseMask = do
  try (string "mask = ")
  mask <- count 36 alphaNum
  return $ MaskInst (Mask mask)

parseProgram :: String -> [ProgramInst]
parseProgram = either (\e -> trace (show e) []) id . parse parser ""
  where
    parser = sepBy (parseMask <|> parseMem) (char '\n')

-- debug
printBit :: Integer -> String
printBit mask = let v = showIntAtBase 2 intToDigit mask "" in replicate (36 - length v) '0' ++ v
