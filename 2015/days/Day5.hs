module Day5
  ( part1
  , part2
  ) where

import           Control.Monad.State  (State, evalState, get, put)
import           Data.Either          (fromRight)
import           Data.Set             (Set, empty, insert, member, singleton)
import           Data.Text            (Text, pack)
import           Data.Void            (Void)
import           Text.Megaparsec      (ParsecT, eof, runParserT, (<|>))
import           Text.Megaparsec.Char (eol, lowerChar)

type ParserState = State (Char, [Char], Bool, Bool)

type Parser = ParsecT Void Text ParserState Int

type BetterState = State (Char, Char, Char, Set String, Bool, Bool)

type BetterParser = ParsecT Void Text BetterState Int

parseFirst :: Parser
parseFirst = do
  firstChar <- lowerChar
  let vowels = updateVowels firstChar []
  put (firstChar, vowels, False, False)
  parseLine

betterParseFirst :: BetterParser
betterParseFirst = do
  firstChar <- lowerChar
  put (firstChar, ' ', ' ', empty, False, False)
  betterParseSecond

betterParseSecond :: BetterParser
betterParseSecond = do
  secondChar <- lowerChar
  (firstChar, _, _, _, _, _) <- get
  put
    ( firstChar
    , secondChar
    , ' '
    , singleton [firstChar, secondChar]
    , False
    , False)
  betterParseThird

betterParseThird :: BetterParser
betterParseThird = do
  thirdChar <- lowerChar
  (firstChar, secondChar, _, pairs, False, False) <- get
  let hasSepPair = thirdChar == firstChar
  put
    ( firstChar
    , secondChar
    , thirdChar
    , insert [secondChar, thirdChar] pairs
    , False
    , hasSepPair)
  betterParseLine

betterParseLine :: BetterParser
betterParseLine = betterParseLetter <|> betterParseEOL

parseLine :: Parser
parseLine = parseLetter <|> parseEOL

parseLetter :: Parser
parseLetter = do
  letter <- lowerChar
  (previous, vowels, hasPairs, hasNaughty) <- get
  let hasPairs' = hasPairs || letter == previous
      hasNaughty' =
        hasNaughty || [previous, letter] `elem` ["ab", "cd", "pq", "xy"]
      vowels' = updateVowels letter vowels
  put (letter, vowels', hasPairs', hasNaughty')
  parseLine

betterParseLetter :: BetterParser
betterParseLetter = do
  letter <- lowerChar
  (firstChar, secondChar, thirdChar, pairs, hasPair, hasSepPair) <- get
  let pair = [thirdChar, letter]
      hasPair' =
        hasPair
          || ((letter /= secondChar
                 || letter /= thirdChar
                 || letter == firstChar)
                && pair `member` pairs)
      hasSepPair' = hasSepPair || letter == secondChar
      pairs' = insert pair pairs
  put (secondChar, thirdChar, letter, pairs', hasPair', hasSepPair')
  betterParseLine

parseEOL :: Parser
parseEOL = do
  eol
  (_, vowels, hasPairs, hasNaughty) <- get
  let result
        | length vowels >= 3 && hasPairs && not hasNaughty = (+ 1)
        | otherwise = id
  result <$> parseNext

betterParseEOL :: BetterParser
betterParseEOL = do
  eol
  (_, _, _, _, hasPair, hasSepPair) <- get
  let result
        | hasPair && hasSepPair = (+ 1)
        | otherwise = id
  result <$> betterParseNext

parseNext :: Parser
parseNext = parseFirst <|> (eof >> return 0)

betterParseNext :: BetterParser
betterParseNext = betterParseFirst <|> (eof >> return 0)

updateVowels :: Char -> [Char] -> [Char]
updateVowels letter vowels
  | letter `elem` "aeiou" = letter : vowels
  | otherwise = vowels

part1 :: Bool -> Text -> String
part1 _ =
  show
    . fromRight (error "parser failed")
    . flip evalState (' ', [], False, False)
    . runParserT parseFirst "Day 5"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . fromRight (error "parser failed")
    . flip evalState (' ', ' ', ' ', empty, False, False)
    . runParserT betterParseFirst "Day 5"
