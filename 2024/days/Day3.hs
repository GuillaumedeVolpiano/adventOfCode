module Day3
  ( part1
  , part2
  ) where

import           Data.Either                (fromRight)
import           Helpers.Parsers            (Parser)
import           Text.Megaparsec            (eof, parse, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, printChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parser [Int]
parseInput =
  try parseMul
    <|> (do
           printChar
           parseInput)
    <|> (do
           eol
           parseInput)
    <|> (do
           eof
           return [])

parseMul :: Parser [Int]
parseMul = do
  string "mul("
  a <- decimal
  char ','
  b <- decimal
  char ')'
  ((a * b) :) <$> parseInput

part1 :: Bool -> String -> String
part1 _ = show . sum . fromRight [] . parse parseInput ""

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
