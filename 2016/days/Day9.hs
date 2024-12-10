module Day9
  ( part1
  , part2
  ) where

import           Control.Monad.State        (State, evalState, get)
import           Data.Char                  (isDigit, isUpper)
import           Data.Either                (fromRight)
import           Data.Text                  as T (Text, length, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParsecT, eof, lookAhead, optional,
                                             runParserT, setOffset, takeP,
                                             takeWhile1P, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, upperChar)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = ParsecT Void Text (State Bool)

getLength :: Parser Int
getLength = T.length <$> takeWhile1P Nothing isUpper

decompress :: Parser Int
decompress =
  parseFlat
    <|> parseInner
    <|> (do
           eol
           return 0)

parseFlat :: Parser Int
parseFlat = (+) <$> getLength <*> decompress

parseInner :: Parser Int
parseInner = do
  char '('
  l <- decimal
  char 'x'
  n <- decimal
  char ')'
  isPart2 <- get
  decompressed <-
    if isPart2
      then do
        val <- decompressN l
        return ((n * val) +)
      else do
        takeP Nothing l
        return ((l * n) +)
  decompressed <$> decompress

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
  let l = read . unpack $ rawL
      c = read . unpack $ rawC
      consumed = 3 + T.length rawL + T.length rawC + l
  val <- decompressN l
  ((c * val) +) <$> decompressN (n - consumed)

part1 :: Bool -> Text -> String
part1 _ = show . fromRight 0 . flip evalState False . runParserT decompress ""

part2 :: Bool -> Text -> String
part2 _ = show . fromRight 0 . flip evalState True . runParserT decompress ""
