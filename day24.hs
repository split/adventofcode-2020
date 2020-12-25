import qualified Data.Map as M
import Data.NumInstances.Tuple ()
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

type Hex = (Int, Int)

type FlipMap = S.Set Hex

{-# INLINE directions #-}
directions :: [(String, Hex)]
directions = [("e", (1, 0)), ("ne", (1, -1)), ("nw", (0, -1)), ("w", (-1, 0)), ("sw", (-1, 1)), ("se", (0, 1))]

flipTile :: FlipMap -> Hex -> FlipMap
flipTile tiles tile
  | tile `notElem` tiles = S.insert tile tiles
  | otherwise = S.delete tile tiles

gameOfTiles :: FlipMap -> FlipMap
gameOfTiles tiles = M.keysSet $ M.union stayedBlack turnedToBlack
  where
    tileM = M.unionsWith (+) [M.fromSet (const 1) (S.mapMonotonic (+ d) tiles) | d <- snd <$> directions]
    turnedToBlack = M.filter (== 2) $ M.withoutKeys tileM tiles
    stayedBlack = M.filter (<= 2) $ M.restrictKeys tileM tiles

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