import Data.Bifunctor (Bifunctor (second), bimap)
import Data.List (intersect, transpose, (\\))
import Debug.Trace
import Text.ParserCombinators.Parsec

sides :: [[a]] -> [[a]]
sides = map head . take 4 . iterate (reverse . transpose)

tilesMatch :: Tile -> Tile -> Bool
tilesMatch a b = not (null common)
  where
    common = (sa ++ map reverse sa) `intersect` (sb ++ map reverse sb)
    sa = sides (snd a)
    sb = sides (snd b)

findMatchingTiles :: [Tile] -> [(Tile, [Tile])]
findMatchingTiles tiles = map (\tile -> (tile, filter (tilesMatch tile) (tiles \\ [tile]))) tiles

type Tile = (Int, [String])

tiles :: Parser [Tile]
tiles = many (try $ (,) <$> tileId <*> tile <* (eof <|> () <$ char '\n'))

tileId :: Parser Int
tileId = read <$> (string "Tile " *> many1 digit <* string ":\n")

tile :: Parser [String]
tile = endBy (try $ many1 (char '.' <|> char '#')) (char '\n')

main :: IO ()
main = interact (either show (show . product . map (fst . fst) . filter ((== 2) . length . snd) . findMatchingTiles) . parse tiles "")