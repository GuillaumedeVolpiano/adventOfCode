module Day3
  ( part1
  , part2
  ) where

import           Data.Either                (fromRight)
import           Helpers.Parsers            (Parser)
import           Text.Megaparsec            (eof, parse, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, printChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Bool -> Parser [(Bool, Int)]
parseInput isDo =
  try (parseMul isDo)
    <|> try
          (do
             string "do()"
             parseInput True)
    <|> try
          (do
             string "don't()"
             parseInput False)
    <|> (do
           printChar
           parseInput isDo)
    <|> (do
           eol
           parseInput isDo)
    <|> (do
           eof
           return [])

parseMul :: Bool -> Parser [(Bool, Int)]
parseMul isDo = do
  string "mul("
  a <- decimal
  char ','
  b <- decimal
  char ')'
  ((isDo, a * b) :) <$> parseInput isDo

part1 :: Bool -> String -> String
part1 _ = show . sum . map snd . fromRight [] . parse (parseInput True) ""

part2 :: Bool -> String -> String
part2 _ =
  show . sum . map snd . filter fst . fromRight [] . parse (parseInput True) ""
