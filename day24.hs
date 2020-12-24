import Data.NumInstances.Tuple
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

type Hex = (Int, Int)

type FlipMap = S.Set Hex

directions :: [(String, Hex)]
directions = [("e", (1, 0)), ("ne", (1, -1)), ("nw", (0, -1)), ("w", (-1, 0)), ("sw", (-1, 1)), ("se", (0, 1))]

dirSet = S.fromList (map snd directions)

flipTile :: FlipMap -> Hex -> FlipMap
flipTile tiles tile = if tile `notElem` tiles then S.insert tile tiles else S.delete tile tiles

gameOfTiles :: FlipMap -> FlipMap
gameOfTiles tiles = S.unions $ S.map processTile tiles
  where
    countBlack = length . S.filter (`elem` tiles)
    adjacent t = S.map (+ t) dirSet
    processTile :: Hex -> S.Set Hex
    processTile t
      | black == 0 || black > 2 = newTiles
      | otherwise = S.insert t newTiles
      where
        newTiles = S.filter ((== 2) . countBlack . adjacent) adj
        black = countBlack adj
        adj = adjacent t

main :: IO ()
main = interact (either show (unlines . sequence [part1, part2] . foldl flipTile S.empty) . parse (hexCoord `sepBy` char '\n') "")
  where
    part1 = ("Part 1: " ++) . show . length
    part2 = ("Part 2: " ++) . show . length . (!! 100) . iterate gameOfTiles

hexCoord :: Parser Hex
hexCoord = sum <$> many directionP

directionP :: Parser Hex
directionP = choice (map toDirParser directions)
  where
    toDirParser :: (String, Hex) -> Parser Hex
    toDirParser (s, d) = d <$ try (string s)