import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import qualified Data.Map as Map
import Debug.Trace
import Text.ParserCombinators.Parsec

data Rule = Match Char | RefR Int | And Rule Rule | Or Rule Rule
  deriving (Show, Eq)

type RuleMap = Map.Map Int Rule

mergeO :: [String] -> [String] -> [String]
mergeO a b = concatMap (\a' -> map (a' ++) b) a

toMatch :: RuleMap -> [String]
toMatch ruleMap = go (RefR 0)
  where
    go (Match c) = [[c]]
    go (RefR i) = go (ruleMap Map.! i)
    go (And a b) = mergeO (go a) (go b)
    go (Or a b) = go a ++ go b

{-
0: 1 2 | 2 3
1: 2 3
2: "b"
3: "c"

ca
-}

-- processRule :: Int -> RuleMap -> Rule

solve :: (RuleMap, [String]) -> Int
solve (ruleMap, messages) = length $ filter validator messages
  where
    validator :: String -> Bool
    validator s = s `elem` toMatch ruleMap

main :: IO ()
main = interact (either show (show . solve) . parse doc "")

doc :: Parser (RuleMap, [String])
doc = do
  rules <- Map.fromList <$> manyTill indexedRule (try (char '\n'))
  messages <- many1 letter `sepBy` char '\n' <* eof
  return (rules, messages)

indexedRule :: Parser (Int, Rule)
indexedRule = (,) <$> int <* string ": " <*> rule <* char '\n'

rule :: Parser Rule
rule = makeExprParser (matchP <|> refP) [[andP], [orP]]
  where
    orP = InfixL $ Or <$ try (string " | ")
    andP = InfixL $ And <$ try (char ' ' <* notFollowedBy (char '|'))
    refP = RefR . read <$> many1 digit
    matchP = Match <$> between (char '"') (char '"') letter

-- match = Match <$> between (char '"') (char '"') letter

int :: Parser Int
int = read <$> try (many1 digit)
