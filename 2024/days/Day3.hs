module Day3
  ( part1
  , part2
  ) where

import           Data.Either                (fromRight)
import           Data.Text                  (Text, pack)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, parse, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, printChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Bool -> Bool -> Parser Int
parseInput isPart1 isDo =
  try (parseMul isPart1 isDo)
    <|> try
          (do
             string . pack $ "do()"
             parseInput isPart1 True)
    <|> try
          (do
             string . pack $ "don't()"
             parseInput isPart1 False)
    <|> (do
           printChar
           parseInput isPart1 isDo)
    <|> (do
           eol
           parseInput isPart1 isDo)
    <|> (do
           eof
           return 0)

parseMul :: Bool -> Bool -> Parser Int
parseMul isPart1 isDo = do
  string . pack $ "mul("
  a <- decimal
  char ','
  b <- decimal
  char ')'
  if isPart1 || isDo
    then ((a * b) +) <$> parseInput isPart1 isDo
    else parseInput isPart1 isDo

part1 :: Bool -> Text -> String
part1 _ = show . fromRight 0 . parse (parseInput True True) ""

part2 :: Bool -> Text -> String
part2 _ = show . fromRight 0 . parse (parseInput False True) ""
