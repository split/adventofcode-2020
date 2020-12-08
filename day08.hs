import Control.Monad
import Control.Monad.State
import Data.String.Utils (replace)
import Relude.List (isPrefixOf, (!!?))

data Mem = Mem
  { history :: [Int],
    acc :: Int,
    excited :: Bool
  }
  deriving (Show)

emptyMem :: Mem
emptyMem = Mem {history = [0], acc = 0, excited = False}

programExcited :: Mem -> Maybe Mem
programExcited program = if excited program then Just program else Nothing

program :: [String] -> State Mem Mem
program prog = do
  state <- get
  maybe (return state) handleResult (evalNextInst prog state)
  where
    handleResult newState = do
      put newState
      if excited newState
        then return newState
        else program prog

evalNextInst :: [String] -> Mem -> Maybe Mem
evalNextInst prog state = case inst of
  Just ('n' : 'o' : 'p' : ' ' : _) -> Just state {history = (cur + 1) : hist}
  Just ('a' : 'c' : 'c' : ' ' : v) -> Just state {history = (cur + 1) : hist, acc = acc state + num v}
  Just ('j' : 'm' : 'p' : ' ' : v) ->
    -- detect infinite loops
    if cur + num v `elem` hist
      then Nothing
      else Just state {history = (cur + num v) : hist}
  Nothing -> if length prog == cur then Just state {excited = True} else Nothing
  where
    hist = history state
    cur = head hist
    inst = prog !!? cur

num :: String -> Int
num ('+' : n) = read n
num ('-' : n) = negate (read n)
num _ = 0

part1 :: [String] -> String
part1 input = "Part 1: " ++ show (acc (evalState (program input) emptyMem))

part2 :: [String] -> String
part2 input = "Part 2: " ++ maybe "no result found" show (evalProgramVariants input)

evalProgramVariants :: [String] -> Maybe Int
evalProgramVariants input = acc <$> msum (evalProgram' <$> corruptionRepairs [] input)
  where
    evalProgram' p = programExcited (evalState (program p) emptyMem)

corruptionRepairs :: [String] -> [String] -> [[String]]
corruptionRepairs _ [] = []
corruptionRepairs past insts = newComb : corruptionRepairs (past ++ [inst]) next
  where
    inst = head insts
    next = if length insts > 1 then tail insts else []
    newComb = past ++ [replaceInst inst] ++ next

replaceInst :: [Char] -> [Char]
replaceInst inst
  | "nop" `isPrefixOf` inst = replace "nop" "jmp" inst
  | "jmp" `isPrefixOf` inst = replace "jmp" "nop" inst
  | otherwise = inst

main :: IO ()
main = interact (unlines . sequence [part1, part2] . lines)