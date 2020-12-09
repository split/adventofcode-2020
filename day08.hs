import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.String.Utils (replace)
import Relude (guarded)
import Relude.List (isPrefixOf, (!!?))

type Program = [String]

data Mem = Mem
  { history :: [Int],
    backups :: [Mem],
    acc :: Int,
    exited :: Bool
  }
  deriving (Show)

emptyMem :: Mem
emptyMem = Mem {history = [0], acc = 0, exited = False, backups = []}

backup :: Mem -> Mem
backup state = state {backups = state : backups state}

runProgram :: Program -> State Mem Mem
runProgram program = do
  state <- get
  maybe (return state) handleResult (evalNextInst program state)
  where
    handleResult newState = do
      put newState
      if exited newState
        then return newState
        else runProgram program

evalNextInst :: Program -> Mem -> Maybe Mem
evalNextInst program state = case inst of
  Just ('n' : 'o' : 'p' : ' ' : _) -> Just (backup state {history = (cur + 1) : hist})
  Just ('a' : 'c' : 'c' : ' ' : v) -> Just state {history = (cur + 1) : hist, acc = acc state + num v}
  Just ('j' : 'm' : 'p' : ' ' : v) -> (\target -> backup state {history = target : hist}) <$> guarded notInfiniteLoop (cur + num v)
  Nothing -> if length program == cur then Just state {exited = True} else Nothing
  where
    hist = history state
    notInfiniteLoop = flip notElem hist
    cur = head hist
    inst = program !!? cur

num :: String -> Int
num ('+' : n) = read n
num ('-' : n) = negate (read n)
num _ = 0

part1 :: Program -> String
part1 program = "Part 1: " ++ show (acc (evalState (runProgram program) emptyMem))

part2 :: Program -> String
part2 program = "Part 2: " ++ maybe "no result found" show (evalWithBackupPatching program)

evalWithBackupPatching :: Program -> Maybe Int
evalWithBackupPatching program = acc <$> msum (restoreBackup <$> backups originalMemory)
  where
    originalMemory = evalState (runProgram program) emptyMem
    restoreBackup state = guarded exited (evalState (runProgram (patchProgram state)) state)
    patchProgram state = program & element (head $ history state) %~ replaceInst

replaceInst :: String -> String
replaceInst inst
  | "nop" `isPrefixOf` inst = replace "nop" "jmp" inst
  | "jmp" `isPrefixOf` inst = replace "jmp" "nop" inst
  | otherwise = inst

main :: IO ()
main = interact (unlines . sequence [part1, part2] . lines)