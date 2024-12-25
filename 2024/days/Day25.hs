module Day25
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (first, second)
import           Data.Either          (fromRight)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (Parser)
import           Text.Megaparsec      (eof, manyTill, parse, someTill, try,
                                       (<|>))
import           Text.Megaparsec.Char (char, eol)

import           Debug.Trace

type Key = [Bool]

type Lock = [Bool]

parseInput :: Parser ([Lock], [Key])
parseInput = try parseSchematics <|> parseLast

parseSchematics :: Parser ([Lock], [Key])
parseSchematics = do
  firstLine <- parseLine
  rest <- concat <$> manyTill parseLine eol
  let schematics = firstLine ++ rest
      op
        | and firstLine = first
        | otherwise = second
  op (schematics :) <$> parseInput

parseLast :: Parser ([Lock], [Key])
parseLast = do
  firstLine <- parseLine
  rest <- concat <$> manyTill parseLine eof
  let schematics = firstLine ++ rest
      result
        | and firstLine = ([schematics], [])
        | otherwise = ([], [schematics])
  return result

parseLine :: Parser [Bool]
parseLine =
  someTill ((char '#' >> return True) <|> (char '.' >> return False)) eol

countFits :: ([Lock], [Key]) -> Int
countFits (locks, keys) = foldr checkFits 0 keys
  where
    checkFits key = (+) . length . filter and . map (fit key) $ locks
    fit = zipWith (\a b -> not a || not b)

part1 :: Bool -> Text -> String
part1 _ =
  show . countFits . fromRight (error "parse failed") . parse parseInput "day25"

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
