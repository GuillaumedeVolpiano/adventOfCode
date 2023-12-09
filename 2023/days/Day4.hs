module Day4
  ( part1
  , part2
  ) where

import           Data.Maybe    (fromJust)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), adjust, replicate,
                                      splitAt, (!?), (><))
import           Data.Set      as St (Set, fromList, intersection, size)
import           Parsers       (complexParser, numbers)

data Card =
  Card Index Winning Have
  deriving (Show)

type Index = Int

type Winning = Set Int

type Have = Set Int

parseLines :: String -> [Card]
parseLines =
    map
    (\((index:_):winning:have:_) ->
       Card
         (read index)
         (fromList . map read $ winning)
         (fromList . map read $ have)) .
  complexParser [":", "\\|"] [numbers, numbers, numbers]

scoreCard :: Card -> Int
scoreCard (Card _ winning have)
  | inter == 0 = 0
  | otherwise = 2 ^ (inter - 1)
  where
    inter = size . intersection have $ winning

totCards :: Seq Int -> [Card] -> Int
totCards numCards [] = sum numCards
totCards numCards (card@(Card index winning have):cs) = totCards newNumCards cs
  where
    totCard = fromJust $ numCards !? (index - 1)
    score = size . intersection have $ winning
    newNumCards
      | score == 0 = numCards
      | otherwise =
        foldr (adjust (+ totCard)) numCards [index .. (index + score - 1)]

part1 :: Bool -> String -> String
part1 _ = show . sum . map scoreCard . parseLines

part2 :: Bool -> String -> String
part2 _ input =
  show . totCards (Sq.replicate (length cards) 1) $ cards
  where
    cards = parseLines input
