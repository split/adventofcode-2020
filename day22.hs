import Data.List (find, maximumBy, (\\))
import Data.Ord (comparing)
import qualified Data.Set as S
import Maybes (fromJust, mapMaybe)
import Text.ParserCombinators.Parsec

data Player = Player {player :: Int, deck :: [Int]} deriving (Show)

instance Eq Player where
  p1 == p2 = player p1 == player p2

type CardHand = (Int, Player)

type CardHistory = S.Set [Int]

doRound :: [Player] -> [Player]
doRound players =
  let turn = mapMaybe pickCard players
   in giveCardsToWinner (winner turn) turn

pickCard :: Player -> Maybe CardHand
pickCard p
  | null (deck p) = Nothing
  | otherwise = Just (head $ deck p, p {deck = drop 1 (deck p)})

winner :: [CardHand] -> Player
winner = snd . maximumBy (comparing fst)

rewardPlayer :: Player -> [Int] -> Player
rewardPlayer p cards = p {deck = deck p ++ cards}

giveCardsToWinner :: Player -> [CardHand] -> [Player]
giveCardsToWinner winner turn = map rewardWinner turn
  where
    rewardWinner (c, p) = if p == winner then rewardPlayer p (ownCardFirst c) else p
    ownCardFirst c = c : ((fst <$> turn) \\ [c])

recursiveCombat :: CardHistory -> [Player] -> Player
recursiveCombat seen players
  | length playersLeft == 1 = fromJust $ find (== head playersLeft) players
  | cards `S.member` seen = rewardPlayer (head players) (deck (players !! 1))
  | all (\(c, p) -> length (deck p) >= c) turn = nextRound (recursiveCombat S.empty (map subPlayer turn))
  | otherwise = nextRound (winner turn)
  where
    nextRound = recursiveCombat (S.insert cards seen) . flip giveCardsToWinner turn
    subPlayer (n, p) = p {deck = take n (deck p)}
    turn = mapMaybe pickCard players
    (cards, playersLeft) = unzip turn

score :: Player -> Int
score = sum . zipWith (*) [1 ..] . reverse . deck

part1 :: [Player] -> String
part1 = ("Part 1: " ++) . show . score . head . fromJust . find ((== 1) . length) . iterate doRound

part2 :: [Player] -> String
part2 = ("Part 2: " ++) . show . score . recursiveCombat S.empty

main :: IO ()
main = interact (either show (unlines . sequence [part1, part2]) . parse decks "")

decks :: Parser [Player]
decks = many1 player
  where
    player = Player . read <$> (string "Player " *> many1 digit <* string ":\n") <*> deck
    deck = try (read <$> many1 digit) `sepEndBy` char '\n' <* (eof <|> () <$ char '\n')
