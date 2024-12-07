module Day7
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (first, second)
import           Data.Either          (fromRight)
import           Data.List            (intersect)
import           Data.List.Split      (splitOneOf)
import           Data.Text            as T (Text, lines, unpack)
import           Helpers.Parsers.Text (Parser)
import           Text.Megaparsec      (eof, lookAhead, parse, try, (<|>))
import           Text.Megaparsec.Char (char, printChar)

hasTLS :: Text -> Bool
hasTLS =
  uncurry (&&)
    . second not
    . fromRight (False, True)
    . parse parseExternalABBA ""

parseExternalABBA :: Parser (Bool, Bool)
parseExternalABBA =
  try (printChar >> printChar >> printChar >> eof >> return (False, False))
    <|> (do
           char '['
           parseInternalABBA)
    <|> do
          a <- printChar
          (b, c, d) <-
            lookAhead $ do
              b <- printChar
              c <- printChar
              d <- printChar
              return (b, c, d)
          first (((a == d) && (b == c)) ||) <$> parseExternalABBA

parseInternalABBA :: Parser (Bool, Bool)
parseInternalABBA = do
  a <- printChar
  (b, c, d) <-
    lookAhead $ do
      b <- printChar
      c <- printChar
      d <- printChar
      return (b, c, d)
  if a == ']'
    then parseExternalABBA
    else second (((a == d) && (b == c)) ||) <$> parseInternalABBA

alternate :: a -> ([a], [a]) -> ([a], [a])
alternate x (others, ones) = (x : ones, others)

hasSSL :: Text -> Bool
hasSSL =
  not
    . null
    . uncurry intersect
    . fromRight ([], [])
    . parse parseExternalABA ""

parseExternalABA :: Parser ([(Char, Char)], [(Char, Char)])
parseExternalABA =
  try (printChar >> printChar >> eof >> return ([], []))
    <|> (do
           char '['
           parseInternalBAB)
    <|> do
          a <- printChar
          (b, c) <-
            lookAhead $ do
              b <- printChar
              c <- printChar
              return (b, c)
          if a == c && a /= b
            then first ((a, b) :) <$> parseExternalABA
            else parseExternalABA

parseInternalBAB :: Parser ([(Char, Char)], [(Char, Char)])
parseInternalBAB = do
  a <- printChar
  (b, c) <-
    lookAhead $ do
      b <- printChar
      c <- printChar
      return (b, c)
  let result
        | a == ']' = parseExternalABA
        | a == c && a /= b = second ((b, a) :) <$> parseInternalBAB
        | otherwise = parseInternalBAB
  result

part1 :: Bool -> Text -> String
part1 _ = show . length . filter hasTLS . T.lines

part2 :: Bool -> Text -> String
part2 _ = show . length . filter hasSSL . T.lines
