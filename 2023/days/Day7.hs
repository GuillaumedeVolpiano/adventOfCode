{-# LANGUAGE TemplateHaskell #-}

module Day7
  ( part1
  , part2
  ) where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import           Data.Char                  (digitToInt)
import           Data.List                  (group, sort, sortBy)
import           FlatParse.Basic            (anyAsciiDecimalInt, char, failed,
                                             isDigit, runParser, satisfy, some,
                                             switch)
import           Helpers.Parsers.ByteString (splitOnSpace)
import           Helpers.Parsers.FlatParse  (Parser, debug, extract)

class GenHand a where
  handToList :: a -> [Card]
  handType :: a -> Type
  pieceCompare :: a -> a -> Ordering

data Card
  = Low Int
  | T
  | J
  | Q
  | K
  | A
  deriving (Ord, Eq)

data Type
  = High
  | One
  | Two
  | Three
  | Full
  | Four
  | Five
  deriving (Ord, Eq, Show)

data Hand =
  Hand Card Card Card Card Card Type
  deriving (Eq)

data NewHand =
  NewHand Card Card Card Card Card Type
  deriving (Eq)

type Bid = Int

instance Ord Hand where
  compare (Hand a b c d e strength) (Hand a' b' c' d' e' strength') =
    compare strength strength'
      `mappend` compare a a'
      `mappend` compare b b'
      `mappend` compare c c'
      `mappend` compare d d'
      `mappend` compare e e'

instance Ord NewHand where
  compare (NewHand a b c d e strength) (NewHand a' b' c' d' e' strength') =
    compare strength strength'
      `mappend` newCompare a a'
      `mappend` newCompare b b'
      `mappend` newCompare c c'
      `mappend` newCompare d d'
      `mappend` newCompare e e'

newCompare :: Card -> Card -> Ordering
newCompare J b
  | b == J = EQ
  | otherwise = LT
newCompare _ J = GT
newCompare a b = compare a b

newHandType :: Card -> Card -> Card -> Card -> Card -> Type
newHandType a b c d e
  | maxGroup + jsize == 5 = Five
  | maxGroup + jsize == 4 = Four
  | maxGroup + jsize == 3 = testFull
  | maxGroup + jsize == 2 = testTwo
  | otherwise = High
    -- there is always a better hand than two pairs if the hands contains a pair
    -- and a joker or no pair and two jokers
  where
    handList = [a, b, c, d, e]
    noJoker = filter (/= J) handList
    groupedNoJoker = grouped noJoker
    maxGroup
      | null noJoker = 0
      | otherwise = maximum groupedNoJoker
    jsize = length . filter (== J) $ handList
      -- a Full is either an actual full or two pairs and a joker. With one pair and
      -- two jokers, we get a four.
    testFull
      | jsize == 1 && notElem 1 groupedNoJoker = Full
      | jsize == 0 && elem 2 groupedNoJoker = Full
      | otherwise = Three
    testTwo
      | sort groupedNoJoker == [1, 2, 2] = Two
      | otherwise = One
    subCompare [] [] = EQ
    subCompare (f:fs) (s:ss)
      | f == s = subCompare fs ss
      | f == J = LT
      | s == J = GT
      | otherwise = compare f s

parseCard :: Parser Card
parseCard =
  $(switch
      [|case _ of
          "T" -> pure T
          "J" -> pure J
          "Q" -> pure Q
          "K" -> pure K
          "A" -> pure A
          _   -> parseLow|])

parseLow :: Parser Card
parseLow = Low . digitToInt <$> satisfy isDigit

parseHand :: Parser Hand
parseHand = do
  a <- parseCard
  b <- parseCard
  c <- parseCard
  d <- parseCard
  e <- parseCard
  $(char ' ')
  let g = grouped [a, b, c, d, e]
      mg = maximum g
      strength
        | mg == 5 = Five
        | mg == 4 = Four
        | mg == 3 && 2 `elem` g = Full
        | mg == 3 = Three
        | sort g == [1, 2, 2] = Two
        | mg == 2 = One
        | otherwise = High
  pure $ Hand a b c d e strength

parseInput :: Parser [(Hand, Bid)]
parseInput =
  some $ do
    hand <- parseHand
    bid <- anyAsciiDecimalInt
    $(char '\n')
    pure (hand, bid)

grouped :: [Card] -> [Int]
grouped = map length . group . sort

handToNew :: Hand -> NewHand
handToNew (Hand a b c d e _) = NewHand a b c d e $ newHandType a b c d e

score :: Ord a => [(a, Bid)] -> Int
score =
  sum
    . zipWith (\b c -> b * snd c) [1 ..]
    . sortBy (\a b -> compare (fst a) (fst b))

pairToNew :: (Hand, Bid) -> (NewHand, Bid)
pairToNew (a, b) = (handToNew a, b)

part1 :: Bool -> ByteString -> String
part1 _ = show . score . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . score . map pairToNew . extract . runParser parseInput
