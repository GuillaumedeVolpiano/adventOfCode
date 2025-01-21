{-# LANGUAGE TemplateHaskell #-}

module Day4
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Maybe                (fromJust)
import           Data.Sequence             as Sq (Seq ((:<|), (:|>)), adjust,
                                                  replicate, splitAt, (!?),
                                                  (><))
import           Data.Set                  as St (Set, fromList, intersection,
                                                  size)
import           FlatParse.Basic           (anyAsciiDecimalInt, char, eof,
                                            isDigit, optional_, runParser,
                                            skipSatisfy, some, string)
import           Helpers.Parsers           (complexParser, numsAsStrings)
import           Helpers.Parsers.FlatParse (Parser, extract)

data Card =
  Card Index Winning Have
  deriving (Show)

type Index = Int

type Winning = Set Int

type Have = Set Int

parseInput :: Parser [Card]
parseInput = some parseLine <* eof

parseLine :: Parser Card
parseLine = do
  some $ skipSatisfy (not . isDigit)
  index <- anyAsciiDecimalInt
  some $ skipSatisfy (not . isDigit)
  winning <-
    fromList <$> some (anyAsciiDecimalInt >>= \x -> some $(char ' ') >> pure x)
  $(char '|')
  some $(char ' ')
  have <-
    fromList
      <$> some
            (anyAsciiDecimalInt >>= \x -> optional_ (some $(char ' ')) >> pure x)
  $(char '\n')
  pure $ Card index winning have

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

part1 :: Bool -> ByteString -> String
part1 _ = show . sum . map scoreCard . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ input = show . totCards (Sq.replicate (length cards) 1) $ cards
  where
    cards = extract . runParser parseInput $ input
