import Data.List (delete, find, intersect, isPrefixOf, transpose, union, (\\))
import Data.List.Utils (uniq)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Text.ParserCombinators.Parsec

type Range = (Int, Int)

data Rule = Rule {name :: String, validate :: Int -> Bool}

instance Show Rule where
  show = name

instance Eq Rule where
  r1 == r2 = name r1 == name r2

type Ticket = [Int]

data Doc = Doc
  { rules :: [Rule],
    myTicket :: Ticket,
    nearbyTickets :: [Ticket]
  }
  deriving (Show)

nearbyErrors :: Doc -> [Int]
nearbyErrors doc = map (sum . filter (not . isValid doc)) (nearbyTickets doc)

validTickets :: Doc -> [Ticket]
validTickets doc = filter (all (isValid doc)) (nearbyTickets doc)

isValid :: Doc -> Int -> Bool
isValid doc value = any (`validate` value) (rules doc)

getFields :: Doc -> [Rule]
getFields doc = solveFields fieldRules
  where
    getRulesFor value = filter (`validate` value) (rules doc)
    getFieldRules values = foldl1 intersect $ map getRulesFor values
    fieldRules = map getFieldRules (transpose (validTickets doc))

solveFields :: [[Rule]] -> [Rule]
solveFields fieldRules
  | length solved == length fieldRules = solved
  | otherwise = solveFields $ map (\field -> if isSolved field then field else field \\ solved) fieldRules
  where
    solved = concat $ filter isSolved fieldRules
    isSolved = (== 1) . length

part1 :: Doc -> String
part1 = ("Part 1: " ++) . show . sum . nearbyErrors

part2 :: Doc -> String
part2 doc = "Part 2: " ++ show (product $ map fst $ departures $ zip (myTicket doc) (getFields doc))
  where
    departures = filter (("departure" `isPrefixOf`) . name . snd)

main :: IO ()
main = interact (either show (unlines . sequence [part1, part2]) . parse parseDoc "")

parseDoc :: Parser Doc
parseDoc = do
  rules <- parseRule `endBy` char '\n'
  string "\nyour ticket:\n"
  myTicket <- parseTicket
  string "\n\nnearby tickets:\n"
  nearbyTickets <- parseTicket `sepBy` char '\n'
  optional eof
  return (Doc {rules = rules, myTicket = myTicket, nearbyTickets = nearbyTickets})

parseTicket :: Parser Ticket
parseTicket = map read <$> many digit `sepBy` char ','

parseRange :: Parser Range
parseRange = do
  start <- many digit
  char '-'
  end <- many digit
  return (read start, read end)

parseRule :: Parser Rule
parseRule = do
  lookAhead (try (noneOf "\n"))
  name <- many (noneOf ":")
  string ": "
  rangeA <- parseRange
  string " or "
  rangeB <- parseRange
  return $ createMinMaxRule name [rangeA, rangeB]

createMinMaxRule :: String -> [(Int, Int)] -> Rule
createMinMaxRule name ranges =
  Rule
    { name = name,
      validate = \value -> any (\(minv, maxv) -> value >= minv && value <= maxv) ranges
    }