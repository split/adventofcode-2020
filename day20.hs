module Main where

import Control.Monad (msum)
import Data.Bifunctor (Bifunctor (first))
import Data.List (find, intersect, tails, transpose, (\\))
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe)
import Text.ParserCombinators.Parsec

data Tile = Tile
  { tileId :: Int,
    image :: [String],
    attached :: [Maybe Tile]
  }
  deriving (Show)

instance Eq Tile where
  a == b = tileId a == tileId b

createTile :: Int -> [String] -> Tile
createTile tileId image = Tile {tileId = tileId, image = image, attached = replicate 4 Nothing}

findMatching :: (Int, String) -> Tile -> Maybe Tile
findMatching (i, side) tile = (\rotated -> tile {image = rotateBack rotated}) <$> find ((== side) . last) (orientations $ image tile)
  where
    rotateBack image = rotate image !! (4 - i `rem` 4)

-- >>> getAttached (createTile 1 ["ab", "cd"], [createTile 2 ["bd", "xx"]])
-- Tile {tileId = 1, image = ["ab","cd"], attached = [Nothing,Just (Tile {tileId = 2, image = ["bx","dx"], attached = [Nothing,Nothing,Nothing,Nothing]}),Nothing,Nothing]}
getAttached :: (Tile, [Tile]) -> Tile
getAttached (tile, matches) = tile {attached = zipWith matchOrientation (attached tile) (zip [0 ..] . map head . orientations $ image tile)}
  where
    matchOrientation a o = msum [a, matcher o]
    matcher o = listToMaybe (mapMaybe (findMatching o) matches)

assembleTile :: [(Tile, [Tile])] -> Tile -> Tile
assembleTile tiles target = baseTile {attached = map (fmap (assembleTile tiles)) (attached baseTile)}
  where
    baseTile = getAttached (target, fromJust $ lookup target tiles)

pickCorner :: [(Tile, [Tile])] -> (Tile, [Tile])
pickCorner = first (\t -> t {image = reverse (image t)}) . head . filter ((== 2) . length . snd)

rotateCornerRightWay :: Tile -> Tile
rotateCornerRightWay tile
  | isJust (attached tile !! 1) && isJust (attached tile !! 2) = tile
  | otherwise = rotateCornerRightWay (rotateTile tile)

mergeTiles :: Tile -> [String]
mergeTiles tile = mergeRows (mergeCols (removeBorders $ image tile) (attached tile)) (attached tile)
  where
    mergeRows acc attached' = case attached' of
      [_, Just t, _, _] -> let ta = attached t in mergeRows (zipWith (++) acc (mergeCols (removeBorders $ image t) ta)) ta
      _ -> acc
    mergeCols acc attached' = case attached' of
      [_, _, Just t, _] -> mergeCols (acc ++ removeBorders (image t)) (attached t)
      _ -> acc

removeBorders = map (init . tail) . init . tail

isMonster :: [String] -> Bool
isMonster = all (== True) . zipWith checkWave (concat monster) . concat
  where
    checkWave '#' w = w == '#'
    checkWave _ _ = True
    monster =
      [ "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      ]

countMonsters :: [String] -> Int
countMonsters = length . filter isMonster . window2d 20 3

tilesMatch :: Tile -> Tile -> Bool
tilesMatch a b = not (null common)
  where
    common = (sa ++ map reverse sa) `intersect` (sb ++ map reverse sb)
    sa = map head (orientations $ image a)
    sb = map head (orientations $ image b)

rotateTile :: Tile -> Tile
rotateTile tile =
  tile
    { image = reverse $ transpose $ image tile,
      attached = take 4 $ drop 1 $ cycle (map (rotateTile <$>) (attached tile))
    }

orientations :: [[a]] -> [[[a]]]
orientations tile = rotations ++ map reverse rotations
  where
    rotations = take 4 $ rotate tile

rotate :: [[a]] -> [[[a]]]
rotate = iterate (reverse . transpose)

findMatchingTiles :: [Tile] -> [(Tile, [Tile])]
findMatchingTiles tiles = map (\tile -> (tile, filter (tilesMatch tile) (tiles \\ [tile]))) tiles

main :: IO ()
main = interact (either show (unlines . sequence [part1, part2] . findMatchingTiles) . parse tiles "")

part1 :: [(Tile, [Tile])] -> String
part1 = ("Part 1: " ++) . show . product . map (tileId . fst) . filter ((== 2) . length . snd)

part2 :: [(Tile, [Tile])] -> String
part2 tiles =
  unlines
    [ "Completed puzzle:",
      unlines completed,
      "monsters: " ++ show monsters,
      "waves: " ++ show waves,
      "Part 2: " ++ show (waves - 15 * monsters)
    ]
  where
    monsters = sum $ map countMonsters $ orientations completed
    waves = length $ filter (== '#') $ concat completed
    completed = mergeTiles $ assembleTile tiles $ rotateCornerRightWay $ getAttached $ pickCorner tiles

tiles :: Parser [Tile]
tiles = many (try $ createTile <$> tileId <*> tile <* (eof <|> () <$ char '\n'))
  where
    tileId = read <$> (string "Tile " *> many1 digit <* string ":\n")
    tile = endBy (try $ many1 (char '.' <|> char '#')) (char '\n')

window2d :: Int -> Int -> [[a]] -> [[[a]]]
window2d x y = concatMap (map transpose . window x . transpose) . window y
  where
    window len = foldr (zipWith (:)) (repeat []) . take len . tails
