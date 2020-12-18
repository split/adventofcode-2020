module Main where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.ParserCombinators.Parsec

data Expr = Mult Expr Expr | Plus Expr Expr | Val Int
  deriving (Show)

parseExpr :: Bool -> Parser Expr
parseExpr equal = makeExprParser (value <|> parens (parseExpr equal)) opTable
  where
    opTable = if equal then [[plus, mult]] else [[plus], [mult]]
    parens = between (char '(') (char ')')
    value = Val . read <$> many1 digit
    plus = InfixL $ Plus <$ char '+'
    mult = InfixL $ Mult <$ char '*'

calc :: Expr -> Int
calc (Mult a b) = calc a * calc b
calc (Plus a b) = calc a + calc b
calc (Val a) = a

-- | Solve expr |
--
-- Examples:
--
-- >>> solveExpr True "1 + (2 * 3) + (4 * (5 + 6))"
-- 51
-- >>> solveExpr False "1 + (2 * 3) + (4 * (5 + 6))"
-- 51
-- >>> solveExpr False "2 * 3 + (4 * 5)"
-- 46
-- >>> solveExpr False "5 + (8 * 3 + 9 + 3 * 4 * 3)"
-- 1445
-- >>> solveExpr False "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
-- 669060
-- >>> solveExpr False "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- 23340
solveExpr equal = either (const 0) calc . parse (parseExpr equal) "" . filter (/= ' ')

solve equal = show . sum . map (solveExpr equal)

main :: IO ()
main = interact (unlines . sequence [("Part 1: " ++) . solve True, ("Part 2: " ++) . solve False] . lines)
