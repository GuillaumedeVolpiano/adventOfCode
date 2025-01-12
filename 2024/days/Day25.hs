module Day25
  ( part1
  , part2
  ) where

import           Data.Bifunctor             (first, second)
import           Data.ByteString            (ByteString)
import           Data.Either                (fromRight)
import           Data.Word8                 (_numbersign, _period)
import           Helpers.Parsers.ByteString (Parser)
import           Text.Megaparsec            (eof, manyTill, parse, someTill,
                                             try, (<|>))
import           Text.Megaparsec.Byte       (char, eol)

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
  someTill
    ((char _numbersign >> return True) <|> (char _period >> return False))
    eol

countFits :: ([Lock], [Key]) -> Int
countFits (locks, keys) = foldr checkFits 0 keys
  where
    checkFits key = (+) . length . filter and . map (fit key) $ locks
    fit = zipWith (\a b -> not a || not b)

part1 :: Bool -> ByteString -> String
part1 _ =
  show . countFits . fromRight (error "parse failed") . parse parseInput "day25"

part2 :: Bool -> ByteString -> String
part2 _ _ = "Part 2"
