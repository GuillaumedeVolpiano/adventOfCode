module Day9
  ( part1
  , part2
  ) where

import           Data.Char            (isDigit, isUpper)
import           Data.Either          (fromRight)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (eof, lookAhead, optional, parse,
                                       setOffset, takeP, takeWhile1P, try,
                                       (<|>))
import           Text.Megaparsec.Char (char, eol, upperChar)

getLength :: Parser Int
getLength = length <$> takeWhile1P Nothing isUpper

getNumber :: Parser Int
getNumber = read <$> takeWhile1P Nothing isDigit

decompress :: Bool -> Parser Int
decompress isPart2 =
  parseFlat isPart2
    <|> parseInner isPart2
    <|> (do
           eol
           return 0)

parseFlat :: Bool -> Parser Int
parseFlat isPart2 = (+) <$> getLength <*> decompress isPart2

parseInner :: Bool -> Parser Int
parseInner False = do
  char '('
  l <- getNumber
  char 'x'
  n <- getNumber
  char ')'
  takeP Nothing l
  ((l * n) +) <$> decompress False
parseInner True = do
  char '('
  l <- getNumber
  char 'x'
  n <- getNumber
  char ')'
  val <- decompressN l
  ((n * val) +) <$> decompress True

decompressN :: Int -> Parser Int
decompressN 0 = return 0
decompressN n = parseFlatN n <|> parseInnerN n

parseFlatN :: Int -> Parser Int
parseFlatN n = do
  v <- takeNUpper n
  (v +) <$> decompressN (n - v)

takeNUpper :: Int -> Parser Int
takeNUpper 0 = return 0
takeNUpper n = try lastOne <|> multiple
  where
    lastOne = do
      upperChar
      lookAhead . char $ '('
      return 1
    multiple = do
      upperChar
      (1 +) <$> takeNUpper (n - 1)

parseInnerN :: Int -> Parser Int
parseInnerN n = do
  char '('
  rawL <- takeWhile1P Nothing isDigit
  char 'x'
  rawC <- takeWhile1P Nothing isDigit
  char ')'
  let l = read rawL
      c = read rawC
      consumed = 3 + length rawL + length rawC + l
  val <- decompressN l
  ((c * val) +) <$> decompressN (n - consumed)

part1 :: Bool -> String -> String
part1 _ = show . fromRight 0 . parse (decompress False) ""

part2 :: Bool -> String -> String
part2 _ = show . fromRight 0 . parse (decompress True) ""
