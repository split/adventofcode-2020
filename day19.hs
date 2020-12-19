import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

data Rule = Match Char | RefR Int | And Rule Rule | Or Rule Rule
  deriving (Show, Eq)

type RuleMap = Map.Map Int Rule

toMatch :: RuleMap -> String -> Bool
toMatch ruleMap = elem "" . go (RefR 0)
  where
    go _ "" = []
    go (Match c) (c' : rest) = [rest | c' == c]
    go (RefR i) s = go (ruleMap Map.! i) s
    go (And a b) s = go a s >>= go b
    go (Or a b) s = go a s ++ go b s

solve :: RuleMap -> [String] -> Int
solve ruleMap = length . filter (toMatch ruleMap)

main :: IO ()
main = interact (either show (unlines . sequence [part1, part2]) . parse doc "")
  where
    part1 (ruleMap, messages) = "Part 1: " ++ show (solve ruleMap messages)
    part2 (ruleMap, messages) = "Part 2: " ++ show (solve (Map.union extras ruleMap) messages)
    extras = Map.fromList [(8, Or (RefR 42) (And (RefR 42) (RefR 8))), (11, Or (And (RefR 42) (RefR 31)) (And (RefR 42) (And (RefR 11) (RefR 31))))]

doc :: Parser (RuleMap, [String])
doc = do
  rules <- Map.fromList <$> manyTill indexedRule (try (char '\n'))
  messages <- many1 letter `sepBy` char '\n' <* eof
  return (rules, messages)

indexedRule :: Parser (Int, Rule)
indexedRule = (,) . read <$> try (many1 digit) <* string ": " <*> rule <* char '\n'

rule :: Parser Rule
rule = makeExprParser (matchP <|> refP) [[andP], [orP]]
  where
    orP = InfixL $ Or <$ try (string " | ")
    andP = InfixL $ And <$ try (char ' ' <* notFollowedBy (char '|'))
    refP = RefR . read <$> many1 digit
    matchP = Match <$> between (char '"') (char '"') letter